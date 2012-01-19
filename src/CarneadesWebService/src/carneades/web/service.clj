;;; Copyright (c) 2011 Fraunhofer Gesellschaft
;;; Licensed under the EUPL V.1.1

(ns carneades.web.service
   (:use clojure.data.json
         clojure.pprint
         compojure.core
         carneades.engine.unify
         carneades.engine.statement
         carneades.engine.argument
         carneades.engine.scheme
         carneades.engine.dublin-core
         carneades.database.db
         carneades.database.admin
         carneades.database.export
         carneades.xml.caf.export
         carneades.web.liverpool-schemes)
  (:require [compojure.route :as route]
            [compojure.handler :as handler]
            [clojure.java.jdbc :as jdbc]
            [carneades.maps.lacij :as lacij]))

;; To Do: 
;; - way to bootstrap the debate database
;; - retrieving user name and password from the request 
;; - search operations, including full text search
;; - CAF import
;; - OPML export
;; - validate input?
 

;(defmacro with-db [db & body]   
;  `(try (jdbc/with-connection 
;           ~db
;           (jdbc/transaction ~@body))
;        (catch Exception t# 
;               {:status 500
;                 :body "The server encountered an unexpected condition which prevented it from fulfilling the request."})))
;

; Use the following version of the macro for testing,
; so that stack traces are printed in the browser:

(defmacro with-db [db & body]   
  `(jdbc/with-connection 
           ~db
           (jdbc/transaction ~@body)))


(defn json-response [data & [status]]
  (if (nil? data)
    {:status 404,
     :body "Not found."}
    {:status (or status 200)     ; 200 is OK
     :headers {"Content-Type" "application/json"}
     :body (json-str data)}))

(defn- pack-statement 
  [stmt]
  (cond (sliteral? stmt) (str stmt),
        (statement? stmt) (assoc stmt :atom (str (literal-atom stmt))),
        :else nil))

(defn- unpack-statement [s]
   (cond (string? s) (binding [*read-eval* false] (read-string s)),  
         (map? s) (assoc (map->statement s) 
                    :atom (binding [*read-eval* false] (read-string (:atom s)))),
        :else nil))

(defn- pack-argument
  [arg]
  (if (nil? arg)
    nil
    (merge arg
           {:scheme (str (:scheme arg)),
            :conclusion (pack-statement (:conclusion arg)),
            :premises (map (fn [p] (assoc p :statement 
                                          (pack-statement (:statement p))))
                           (:premises arg))})))

(defn- unpack-argument [arg]
  (assoc arg
         :scheme (symbol (:scheme arg))
         :conclusion (unpack-statement (:conclusion arg))
         :premises (map (fn [p] (assoc p :statement (unpack-statement (:statement p))))
                        (:premises arg))))

(defn- unpack-subs 
  "Replace keywords by logical variables in the substitutions
   received from Web clients."
  [m]
  (zipmap (map (fn [key] (symbol (name key)))
               (keys m))
          (vals m)))

(defn argument-data
  "Returns the argument content"
  [id]
  {:pre [(symbol? id)]}
  (pack-argument (read-argument (str id))))

(defn- argument-metadata
  "Returns the metadata of an argument in a map
   or an empty map of if the argument has no metadata"
  [id]
  {:pre [(symbol? id)]}
  (or (:header (argument-data id))
      {}))

(defn cut-ag-helper
  [n depth]
  (cond (= depth 1)
        [n []]

        (statement? n)
        (let [procon (concat (map argument-data (:pro n)) (map argument-data (:con n)))]
          [n (vec (map #(cut-ag-helper % (dec depth)) procon))])

        (argument? n)
        (let [premises (map :statement (:premises n))]
          [n (vec (map #(cut-ag-helper % (dec depth)) premises))])))

(defn cut-ag
  [depth]
  [:root (vec (map #(cut-ag-helper % depth) (main-issues)))])

;; We don't use the defroutes macro.
;; This allow handlers to be reused in another project

(def carneades-web-service-routes
     
     [ 
      
      ;; Debates
           
      (GET "/debate" [] 
           (let [db2 (make-database-connection "debates" "guest" "")]
             (with-db db2 (json-response (list-debates)))))
      
      (GET "/debate/:id" [id]
           (let [db2 (make-database-connection "debates" "guest" "")]
             (with-db db2 (json-response (read-debate id)))))
      
      (POST "/debate" request
            (let [m (read-json (slurp (:body request)))
                  db (make-database-connection "debates" "root" "pw1")]
              (with-db db (json-response (create-debate m))))) 
      
      (PUT "/debate" request   
           (let [m (read-json (slurp (:body request)))
                 db (make-database-connection "debates" "root" "pw1")
                 id (:id (:params request))]
             (with-db db (json-response (update-debate id m))))) 
      
      ; To Do: Deleting debates
       
      ;; Metadata        
      (GET "/metadata/:db" [db] 
           (let [db2 (make-database-connection db "guest" "")]
             (with-db db2 (json-response (list-metadata)))))
      
      (GET "/metadata/:db/:id" [db id]
           (let [db2 (make-database-connection db "guest" "")]
             (with-db db2 (json-response (read-metadata id)))))
      
      (POST "/metadata" request
            (let [m (read-json (slurp (:body request)))
                  db (make-database-connection (:db (:params request)) "root" "pw1")]
              (with-db db (json-response (create-metadata (map->metadata m)))))) 
      
      (PUT "/metadata" request   
           (let [m (read-json (slurp (:body request)))
                 db (make-database-connection (:db (:params request)) "root" "pw1")
                 id (java.lang.Integer/parseInt (:id (:params request)))]
             (with-db db (json-response (update-metadata id m))))) 
      
      (DELETE "/metadata/:db/:id" [db id] 
              (let [db2 (make-database-connection db "root" "pw1")]
                (with-db db2 (json-response (delete-metadata (java.lang.Integer/parseInt id))))))
      
      ;; Statements
      
      (GET "/statement/:db" [db] 
           (let [db2 (make-database-connection db "guest" "")]
             (with-db db2 (json-response (map pack-statement (list-statements))))))  
      
      (GET "/main-issues/:db" [db]
          (let [db2 (make-database-connection db "guest" "")]
             (with-db db2 (json-response (map pack-statement (main-issues))))))               
      
      (GET "/statement/:db/:id" [db id] 
           (let [db2 (make-database-connection db "guest" "")]
             (with-db db2 
               (json-response (pack-statement (read-statement id))))))
      
      (GET "/matching-statements/" request
        ; returns a vector of {:substitutions :statement} records for the statements
        ; with atoms matching the query in the body of the request
        (let [m (read-json (slurp (:body request)))
              s1 (unpack-statement m)
              db (make-database-connection (:db (:params request)) "guest" "")]
              (with-db db (json-response (mapcat (fn [s2] 
                                                   (let [subs (unify (:atom s1) (:atom s2))]
                                                     (when subs 
                                                       [{:substitutions subs
                                                         :statement s2}])))
                                                 (list-statements))))))
      
      
      (GET "/premise-of/:db/:id" [db id]
        ; returns a vector of arguments in which the statement with the given id
        ; is a premise
        (let [db2 (make-database-connection db "guest" "")]  
          (with-db db2 
            (json-response (map (fn [arg-id] (pack-argument (read-argument arg-id)))
                                (:premise-of (read-statement id)))))))
            
      (POST "/statement/:db" request  
            (let [m (read-json (slurp (:body request)))
                  s (unpack-statement m)
                  db (make-database-connection (:db (:params request)) "root" "pw1")]
              (with-db db (json-response (create-statement (map->statement s))))))
      
      (PUT "/statement" request  
           (let [m (read-json (slurp (:body request)))
                 s (unpack-statement m)
                 db (make-database-connection (:db (:params request)) "root" "pw1")
                 id (:id (:params request))]
             (with-db db (json-response (update-statement db id s)))))        
      
      (DELETE "/statement/:db/:id" [db id] 
              (let [db2 (make-database-connection db "root" "pw1")]
                (with-db db2 (json-response (delete-statement id)))))
      
      ;; Arguments  
      
      (GET "/argument/:db" [db]
           (let [db2 (make-database-connection db "guest" "")] 
             (with-db db2 (json-response (map pack-argument (list-arguments))))))
      
      (GET "/argument/:db/:id" [db id]
           (let [db2 (make-database-connection db "guest" "")]  
             (with-db db2 (json-response (pack-argument (read-argument id))))))
      
      (GET "/pro-arguments/:db/:id" [db id]
           (let [db2 (make-database-connection db "guest" "")]  
             (with-db db2 (json-response (get-pro-arguments id)))))
      
      (GET "/con-arguments/:db/:id" [db id]
           (let [db2 (make-database-connection db "guest" "")]  
             (with-db db2 (json-response (get-con-arguments id)))))
      
      (GET "/rebuttals/:db/:id" [db id]
           (let [db2 (make-database-connection db "guest" "")]  
             (with-db db2 (json-response (get-rebuttals id)))))

      (GET "/undercutters/:db/:id" [db id]
           (let [db2 (make-database-connection db "guest" "")]  
             (with-db db2 (json-response (get-undercutters id)))))
      
      (GET "/dependents/:db/:id" [db id]
        ; returns a vector of arguments in which the conclusion of the argument
        ; with the given id is a premise
        (let [db2 (make-database-connection db "guest" "")]  
          (with-db db2 
              (json-response (map (fn [arg-id] (pack-argument (read-argument arg-id))) 
                                  (get-dependents id))))))
      
      (POST "/argument" request  
            (let [m (read-json (slurp (:body request)))
                  arg (unpack-argument m)
                  db (make-database-connection (:db (:params request)) "root" "pw1")]
              (with-db db (json-response (create-argument (map->argument arg))))))
      
      (PUT "/argument" request  
           (let [m (read-json (slurp (:body request)))
                 arg (unpack-argument m)
                 db (make-database-connection (:db (:params request)) "root" "pw1")
                 id (:id (:params request))]
             (with-db db (json-response (update-argument id arg)))))   
      
      (DELETE "/argument/:db/:id" [db id] 
              (let [db2 (make-database-connection db "root" "pw1")]
                (with-db db2
                  (json-response (delete-argument id)))))
      
      ;; Namespaces
      
      (GET "/namespace/:db" [db]
           (let [db2 (make-database-connection db "guest" "")] 
             (with-db db2 (json-response (list-namespaces)))))
      
      (GET "/namespace/:db/:prefix" [db prefix]
           (let [db2 (make-database-connection db "guest" "")]  
             (with-db db2 (json-response (read-namespace prefix)))))

      (POST "/namespace/" request  
            (let [prefix (:prefix (:params request)),
                  uri (read-json (slurp (:body request))),
                  db (make-database-connection (:db (:params request)) "root" "pw1")]
              (with-db db (json-response (create-namespace db prefix uri)))))
      
      (PUT "/namespace/" request  
           (let [prefix (:prefix (:params request)),
                 uri (read-json (slurp (:body request))),
                 db (make-database-connection (:db (:params request)) "root" "pw1")]
             (with-db db (json-response (update-namespace prefix uri)))))
      
      (DELETE "/namespace/:db/:prefix" [db prefix] 
              (let [db2 (make-database-connection db "root" "pw1")]
                (with-db db2
                  (json-response (delete-namespace prefix)))))
      
      ;; Statement Polls
      
      (GET "/statement-poll/:db" [db]
           (let [db2 (make-database-connection db "root" "pw1")] 
             (with-db db2 (json-response (list-statement-poll)))))
      
      (GET "/statement-poll/:db/:id" [db id]
           (let [db2 (make-database-connection db "guest" "")]  
             (with-db db2 (json-response (read-statement-poll (java.lang.Integer/parseInt id))))))

      (POST "/statement-poll" request  
            (let [userid (:id (:params request)),
                  votes (read-json (slurp (:body request))),
                  db (make-database-connection (:db (:params request)) "root" "pw1")]
              (with-db db (json-response (create-statement-poll userid votes)))))
      
      (PUT "/statement-poll" request  
           (let [userid (:id (:params request)),
                 votes (read-json (slurp (:body request)))
                 db (make-database-connection (:db (:params request)) "root" "pw1")]
             (with-db db (json-response (update-statement-poll userid votes)))))
      
      (DELETE "/statement-poll/:db/:id" [db id] 
              (let [db2 (make-database-connection db "root" "pw1")]
                (with-db db2
                  (json-response (delete-statement-poll (java.lang.Integer/parseInt id)))))) 
      
      ;; Argument Polls
      
      (GET "/argument-poll/:db" [db]
           (let [db2 (make-database-connection db "root" "pw1")] 
             (with-db db2 (json-response (list-argument-poll)))))
      
      (GET "/argument-poll/:db/:id" [db id]
           (let [db2 (make-database-connection db "guest" "")]  
             (with-db db2 (json-response (read-argument-poll (java.lang.Integer/parseInt id))))))

      (POST "/argument-poll" request  
            (let [userid (:id (:params request)),
                  votes (read-json (slurp (:body request))),
                  db (make-database-connection (:db (:params request)) "root" "pw1")]
              (with-db db (json-response (create-argument-poll userid votes)))))
      
      (PUT "/argument-poll" request  
           (let [userid (:id (:params request)),
                 votes (read-json (slurp (:body request)))
                 db (make-database-connection (:db (:params request)) "root" "pw1")]
             (with-db db (json-response (update-argument-poll userid votes)))))
      
      (DELETE "/argument-poll/:db/:id" [db id] 
              (let [db2 (make-database-connection db "root" "pw1")]
                (with-db db2
                  (json-response (delete-argument-poll (java.lang.Integer/parseInt id))))))

      ;; Aggregated information
      
      (GET "/argumentgraph-info/:db" [db]
           (let [dbconn (make-database-connection db "guest" "")]
             (with-db dbconn
               (let [metadata (list-metadata)
                     main-issues (map pack-statement (main-issues))
                     outline (cut-ag 5)]
                 (json-response {:metadata metadata
                                 :main-issues main-issues
                                 :outline outline})))))

      (GET "/statement-info/:db/:id" [db id]
           (let [dbconn (make-database-connection db "guest" "")]
             (with-db dbconn
               (let [stmt (pack-statement (read-statement id))
                     pro-data (map argument-data (:pro stmt))
                     con-data (map argument-data (:con stmt))
                     premise-of-data (map argument-data (:premise-of stmt))]
                 (json-response (assoc stmt 
                                       :pro-data pro-data 
                                       :con-data con-data
                                       :premise-of-data premise-of-data))))))

      (GET "/argument-info/:db/:id" [db id]
           (let [dbconn (make-database-connection db "guest" "")]
             (with-db dbconn
               (let [arg (pack-argument (read-argument id))
                     undercutters-data (map argument-data (:undercutters arg))
                     rebuttals-data (map argument-data (:rebuttals arg))
                     dependents-data (map argument-data (:dependents arg))]
                 (json-response (assoc arg
                                  :undercutters-data undercutters-data
                                  :rebuttals-data rebuttals-data
                                  :dependents-data dependents-data))))))
      
      
      ;; XML
      
      (GET "/export/:db" [db]
           (let [dbconn (make-database-connection db "guest" "")]
             (with-db dbconn
                      (let [arg-graph (export-to-argument-graph dbconn)
                            xml (with-out-str (argument-graph->xml arg-graph))]
                        (if (nil? xml)
                          {:status 404,
                           :body "Not found."}
                          {:status 200            ; 200 is OK
                           :headers {"Content-Type" "application/xml"}
                           :body xml})))))

      ;; SVG Maps

      (GET "/map/:db" {params :params}
           (let [db (:db params)
                 options (dissoc params :db)
                 dbconn (make-database-connection db "guest" "")]
             (with-db dbconn
               (let [convert-option (fn [val]
                                      (try
                                        (Integer/parseInt val)
                                        (catch Exception _
                                          (keyword val))))
                     ag (export-to-argument-graph dbconn)
                     optionsseq (mapcat (fn [[k v]] [k (convert-option v)]) options)
                     svg (apply lacij/export-str ag optionsseq)]
                 {:status 200
                  :headers {"Content-Type" "image/svg+xml;charset=UTF-8"}
                  :body svg}))))
      
      ;; Schemes
      
      (GET "/scheme" []  ; return all Liverpool schemes
          (json-response (vals liverpool-schemes-by-id)))
      
      (GET "/scheme/:id" [id]  ; return the scheme with the given id
          (json-response (get liverpool-schemes-by-id (symbol id))))
      
      (POST "/matching-schemes" request ; return all schemes with conclusions matching a goal
       (let [goal (unpack-statement (read-json (slurp (:body request))))]
         (json-response (get-schemes liverpool-schemes-by-predicate goal {} true))))
      
      (POST "/apply-scheme/:db/:id" request 
       ; apply the scheme with the given id to the substitutions in the body
       ; and add the resulting arguments, if they are ground, to the 
       ; database. Returns a list of the ids of the new arguments.
       (let [subs (unpack-subs (read-json (slurp (:body request)))),
             scheme (get liverpool-schemes-by-id (symbol (:id (:params request))))]
         (prn subs)
          (let [responses (instantiate-scheme scheme subs)
                dbconn (make-database-connection (:db (:params request)) "root" "pw1")]
             (prn responses)
             (with-db dbconn
                 (json-response 
                   (reduce (fn [l r]
                              (assume (:assumptions r))
                              (conj l (create-argument (:argument r))))
                           []
                           responses))))))
      
      ;; TO DO: command for retreiving the language of the theory
      
       
      ;; Other 
      
      (GET "/" [] 
           "<h1>Carneades Web Service</h1>
            <p>This web service is part of the <a href=\"http://carneades.github.com\">
            Carneades Argumentation System</a></p>")                                                                
      (route/resources "/")
      (route/not-found "Page not found.")]) 

(def carneades-web-service
  (handler/site (apply routes carneades-web-service-routes)))
