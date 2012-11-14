;;; Copyright (c) 2011 Fraunhofer Gesellschaft
;;; Licensed under the EUPL V.1.1

(ns carneades.web.service
   (:use clojure.pprint
         [compojure.core]
         (carneades.engine uuid policy unify statement argument scheme dublin-core utils
                           argument-evaluation aspic)
         carneades.database.db
         carneades.database.admin
         carneades.database.export
         carneades.database.import
         carneades.xml.caf.export
         carneades.web.walton-schemes
         ring.util.codec)
  (:require [clojure.data.json :as json]
            [compojure.route :as route]
            [compojure.handler :as handler]
            [clojure.java.jdbc :as jdbc]
            [clojure.string :as str]
            [carneades.maps.lacij :as lacij]))

;; To Do: 
;; - way to bootstrap the debate database
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
     :body (json/json-str data)}))

(defn zip-metadata-element
  "Zips a metadata element vector as a string"
  [element]
  (if (or (vector? element) (seq? element))
   (if (empty? element)
     nil
     (str/join ";" element))
   element))

(defn unzip-metadata-element
  "Unzips a metadata element string as a vector"
  [s]
  (if (string? s)
    (if (empty? s)
      nil
      (str/split s #";"))
    s))

(defn zip-metadata
  "Zips a map of metadata elements vector and converts it
   to a map of metadata elements strings"
  [md]
  (reduce (fn [md [k v]]
            (if (= k :description)
              md
              (assoc md k (zip-metadata-element v))))
          md
          md))

(defn unzip-metadata
  "Unzips a map of metadata elements string and converts it
   to a map of metadata elements vectors"
  [md]
  (reduce (fn [md [k v]]
            (if (= k :description)
              md
              (assoc md k (unzip-metadata-element v))))
          md
          md))

(defn pack-statement 
  [stmt]
  {:post [(not (vector? (:atom %)))
          (not (seq? (:atom %)))]}
  (cond (sliteral? stmt) (str stmt),
        (statement? stmt) (assoc stmt
                            :atom (when (:atom stmt)
                                    (str (literal-atom stmt)))
                            :header (unzip-metadata (:header stmt)))
        :else nil))

(defn unpack-statement
  "Converts a JSON string representing a statement to a statement object."
  [s]
  (cond (string? s) (safe-read-string s),  
        (map? s) (let [atomval (if (or (nil? (:atom s))
                                       (empty? (:atom s)))
                                 nil
                                 (safe-read-string (:atom s)))]
                   (assoc (map->statement (dissoc s :atom))
                     :standard (keyword (:standard s))
                     :atom atomval
                     :header (map->metadata (zip-metadata (:header s)))))
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
                           (:premises arg))
            :header (unzip-metadata (:header arg))})))

(defn unpack-argument [arg]
  (assoc arg
         :scheme (when (:scheme arg) (symbol (:scheme arg)))
         :conclusion (unpack-statement (:conclusion arg))
         :premises (map (fn [p]
                          (map->premise (assoc p :statement (unpack-statement (:statement p)))))
                        (:premises arg))
         :exceptions (map (fn [p]
                          (map->premise (assoc p :statement (unpack-statement (:statement p)))))
                          (:exceptions arg))
         :header (map->metadata (zip-metadata (:header arg)))))

(defn unpack-arg-attrs
  [attrs]
  (if (:header attrs)
    (assoc attrs :header (map->metadata (:header attrs)))
    attrs))

(defn- unpack-subs 
  "Replace keywords by logical variables in the substitutions
   received from Web clients."
  [m]
  (zipmap (map (fn [key] (symbol (name key)))
               (keys m))
          (map safe-read-string (vals m))))

(defn argument-data
  "Returns the argument content"
  [id]
  {:pre [(or (symbol? id)
             (string? id))]}
  (pack-argument (read-argument (str id))))

(defn- argument-metadata
  "Returns the metadata of an argument in a map
   or an empty map of if the argument has no metadata"
  [id]
  {:pre [(symbol? id)]}
  (or (:header (argument-data id))
      {}))

(defn create-outline-helper
  [n depth]
  (cond (= depth 1)
        [n []]

        (statement? n)
        (let [procon (concat (map argument-data (:pro n)) (map argument-data (:con n)))]
          [n (vec (map #(create-outline-helper % (dec depth)) procon))])

        (argument? n)
        (let [premises (map (comp pack-statement :statement) (:premises n))]
          [n (vec (map #(create-outline-helper % (dec depth)) premises))])))

(defn create-outline
  [depth]
  [:root (vec (map #(create-outline-helper % depth) (map pack-statement (main-issues))))])

(defn get-username-and-password
  [request]
  (let [authorization (second (str/split (get-in request [:headers "authorization"]) #" +"))
        authdata (String. (base64-decode authorization))]
    (str/split authdata #":")))

;; We don't use the defroutes macro.
;; This allow handlers to be reused in another project

(defroutes carneades-web-service-routes
      
  ;; Debates
           
  (GET "/debate" [] 
       (let [db2 (make-database-connection "debates" "guest" "")]
         (with-db db2 (json-response (list-debates)))))
      
  (GET "/debate/:id" [id]
       (let [db2 (make-database-connection "debates" "guest" "")]
         (with-db db2 (json-response (read-debate id)))))
      
  (POST "/debate" request
        (let [m (json/read-json (slurp (:body request)))
              [username password] (get-username-and-password request)
              db (make-database-connection "debates" username password)]
          (with-db db (json-response (create-debate m))))) 
      
  (PUT "/debate/:id" request   
       (let [m (json/read-json (slurp (:body request)))
             [username password] (get-username-and-password request)
             db (make-database-connection "debates" username password)
             id (:id (:params request))]
         (with-db db (json-response (update-debate id m))))) 
      
                                        ; To Do: Deleting debates
       
  ;; Metadata        
  (GET "/metadata/:db" [db] 
       (let [db2 (make-database-connection db "guest" "")]
         (with-db db2 (json-response (list-metadata)))))
      
  (GET "/metadata/:db/:id" [db id]
       (let [db2 (make-database-connection db "guest" "")]
         (with-db db2 (json-response
                       (unzip-metadata
                        (read-metadata id))))))
      
  (POST "/metadata/:db" request
        (let [db (:db (:params request))
              m (json/read-json (slurp (:body request)))
              [username password] (get-username-and-password request)
              dbconn (make-database-connection db username password)]
          (with-db dbconn (json-response
                           {:id (create-metadata
                                 (zip-metadata
                                  (map->metadata m)))}))))

  (PUT "/metadata/:db/:id" request   
       (let [m (json/read-json (slurp (:body request)))
             m (zip-metadata m)
             [username password] (get-username-and-password request)
             db (make-database-connection (:db (:params request)) username password)
             id (Integer/parseInt (:id (:params request)))]
         (with-db db (json-response (do
                                      (update-metadata id m)
                                      (let [data (read-metadata id)
                                            x (unzip-metadata data)]
                                        (prn "data: " data)
                                        (prn "unzipped: " x)
                                        x))))))
      
  (DELETE "/metadata/:db/:id" request
          (let [[username password] (get-username-and-password request)
                dbname (:db (:params request))
                dbconn (make-database-connection dbname username password)
                id (:id (:params request))]
            (with-db dbconn (json-response (delete-metadata (Integer/parseInt id))))))
      
  ;; Statements
      
  (GET "/statement/:db" [db] 
       (let [db2 (make-database-connection db "guest" "")]
         (with-db db2 (json-response (map pack-statement (list-statements))))))  
      
  (GET "/statement/:db/:id" [db id] 
       (let [db2 (make-database-connection db "guest" "")]
         (with-db db2 
           (json-response (pack-statement (read-statement id))))))
            
  (POST "/statement/:db" request  
        (let [m (json/read-json (slurp (:body request)))
              s (unpack-statement m)
              [username password] (get-username-and-password request)
              db (make-database-connection (:db (:params request)) username password)]
          (with-db db (json-response
                       {:id (create-statement s)}))))
      
  (PUT "/statement/:db" request  
       (let [m (json/read-json (slurp (:body request)))
             ;; s (unpack-statement m)
             [username password] (get-username-and-password request)
             db (make-database-connection (:db (:params request)) username password)
             id (:id m)
             m (assoc m :header (zip-metadata (:header m)))]
         (with-db db (json-response (do
                                      (update-statement id (dissoc m :id))
                                      (pack-statement (read-statement id)))))))
      
  (DELETE "/statement/:db/:id" request
          (let [[username password] (get-username-and-password request)
                db (:db (:params request))
                id (:id (:params request))
                db2 (make-database-connection db username password)]
            (with-db db2 (json-response
                          (let [stmt (read-statement id)]
                            (doseq [id (premises-for-statement id)]
                              (delete-premise id))
                            (doseq [pro (:pro stmt)]
                              (delete-argument pro))
                            (doseq [con (:con stmt)]
                              (delete-argument con))
                            (delete-statement id))))))
            
  (GET "/main-issues/:db" [db]
       (let [db2 (make-database-connection db "guest" "")]
         (with-db db2 (json-response (map pack-statement (main-issues))))))               

  (POST "/matching-statements/:db" request
        ;; returns a vector of {:substitutions :statement} records for the statements
        ;; with atoms matching the query in the body of the request
        (let [m (json/read-json (slurp (:body request)))
              db (:db (:params request))
              s1 (unpack-statement m)
              db (make-database-connection (:db (:params request)) "guest" "")]
          (with-db db (json-response (mapcat (fn [s2]
                                               ;; (prn "unifying s1 against s2: " s1 " " (:atom  s2))
                                               (let [subs (unify s1 (:atom s2))]
                                                 ;; (prn "subs = " subs)
                                                 (when subs 
                                                   [{:substitutions subs
                                                     :statement (pack-statement s2)}])))
                                             (list-statements))))))

  (GET "/premise-of/:db/:id" [db id]
                                        ; returns a vector of arguments in which the statement with the given id
                                        ; is a premise
       (let [db2 (make-database-connection db "guest" "")]  
         (with-db db2 
           (json-response (map (fn [arg-id] (pack-argument (read-argument arg-id)))
                               (:premise-of (read-statement id)))))))

      
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
      
  (POST "/argument/:db" request  
        (let [m (json/read-json (slurp (:body request)))
              arg (unpack-argument m)
              [username password] (get-username-and-password request)
              db (make-database-connection (:db (:params request)) username password)
              argument (map->argument arg)
              undercutters (make-undercutters argument)]
          ;; TODO: assumptions?
          (with-db db
            (let [id (create-argument argument)]
              (json-response
               {:id id
                :arguments (cons id
                                 (map (fn [undercutter]
                                        (create-argument undercutter))
                                      undercutters))})))))
      
  (PUT "/argument/:db" request
       (let [m (json/read-json (slurp (:body request)))
             [username password] (get-username-and-password request)
             db (make-database-connection (:db (:params request)) username password)
             id (:id m)
             arg (unpack-argument m)
             arg2 (dissoc arg :id :undercutters :dependents
                         :exceptions :rebuttals)]
         (with-db db (json-response
                      (let [responses (generate-exceptions arg)
                            exceptions-ids (reduce (fn [ids response]
                                                     (conj ids (create-argument (:argument response))))
                                                   []
                                                   responses)]
                        ;; here we have the exceptions' ids but we can not pass them back
                        ;; since backbone.js expects the argument record to be returned...
                        (update-argument id arg2)
                        (argument-data id))))))
      
  (DELETE "/argument/:db/:id" request
          (let [[username password] (get-username-and-password request)
                id (:id (:params request))
                db (:db (:params request))
                db2 (make-database-connection db username password)]
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
              uri (json/read-json (slurp (:body request))),
              [username password] (get-username-and-password request)
              db (make-database-connection (:db (:params request)) username password)]
          (with-db db (json-response (create-namespace db prefix uri)))))
      
  (PUT "/namespace/:db" request  
       (let [prefix (:prefix (:params request)),
             uri (json/read-json (slurp (:body request))),
             [username password] (get-username-and-password request)
             db (make-database-connection (:db (:params request)) username password)]
         (with-db db (json-response (update-namespace prefix uri)))))
      
  (DELETE "/namespace/:db/:prefix" request
          (let [[username password] (get-username-and-password request)
                db (:db (:params request))
                prefix (:prefix (:params request))
                db2 (make-database-connection db username password)]
            (with-db db2
              (json-response (delete-namespace prefix)))))
      
  ;; Statement Polls
      
  (GET "/statement-poll/:db" request
       (let [[username password] (get-username-and-password request)
             db (:db (:params request))
             db2 (make-database-connection db username password)]
         (with-db db2
           (let [ids (set (map :userid (list-statement-poll)))
                 polls (map read-statement-poll ids)
                 polls (if (nil? polls)
                         ()
                         polls)]
             (json-response polls)))))
      
  (GET "/statement-poll/:db/:id" [db id]
       (let [db2 (make-database-connection db "guest" "")]  
         (with-db db2 (json-response (read-statement-poll
                                      (Integer/parseInt id))))))

  (POST "/statement-poll/:db" request  
        (let [poll (json/read-json (slurp (:body request))),
              [username password] (get-username-and-password request)
              db (make-database-connection (:db (:params request)) username password)]
          (with-db db
            (do
              (create-statement-poll poll)
              (json-response (read-statement-poll (:id poll)))))))
      
  (PUT "/statement-poll/:db" request  
       (let [poll (json/read-json (slurp (:body request)))
             [username password] (get-username-and-password request)
             db (make-database-connection (:db (:params request)) username password)]
         (with-db db
           (do
             (update-statement-poll poll)
             (json-response (read-statement-poll (:id poll)))))))
      
  (DELETE "/statement-poll/:db/:id" request
          (let [[username password] (get-username-and-password request)
                db (:db (:params request))
                id (:id (:params request))
                db2 (make-database-connection db username password)]
            (with-db db2
              (json-response (delete-statement-poll (Integer/parseInt id)))))) 
      
  ;; Argument Polls
      
  (GET "/argument-poll/:db" request
       (let [[username password] (get-username-and-password request)
             db (:db (:params request))
             db2 (make-database-connection db username password)] 
         (with-db db2 (let [ids (set (map :userid (list-argument-poll)))
                            polls (map read-argument-poll ids)
                            polls (if (nil? polls)
                                    ()
                                    polls)]
                        (json-response polls)))))
      
  (GET "/argument-poll/:db/:id" request
       (let [[username password] (get-username-and-password request)
             db (:db (:params request))
             id (:db (:params request))
             dbconn (make-database-connection (:db (:params request)) username password)]  
         (with-db dbconn
           (json-response (read-argument-poll (Integer/parseInt id))))))

  (POST "/argument-poll/:db" request  
        (let [poll (json/read-json (slurp (:body request))),
              [username password] (get-username-and-password request)
              db (make-database-connection (:db (:params request)) username password)]
          (with-db db
            (do (create-argument-poll poll)
                (json-response (read-argument-poll (:id poll)))))))
      
  (PUT "/argument-poll/:db" request  
       (let [poll (json/read-json (slurp (:body request)))
             [username password] (get-username-and-password request)
             db (make-database-connection (:db (:params request)) username password)]
         (with-db db
           (do (update-argument-poll poll)
               (json-response (read-argument-poll (:id poll)))))))
      
  (DELETE "/argument-poll/:db/:id" request
          (let [[username password] (get-username-and-password request)
                db (:db (:params request))
                id (:id (:params request))
                db2 (make-database-connection db username password)]
            (with-db db2
              (json-response (delete-argument-poll (Integer/parseInt id))))))

  ;; Aggregated information
      
  (GET "/argumentgraph-info/:db" [db]
       (let [dbconn (make-database-connection db "guest" "")]
         (with-db dbconn
           (let [metadata (list-metadata)
                 main-issues (map pack-statement (main-issues))
                 outline (create-outline 5)]
             (json-response {:metadata (map unzip-metadata metadata)
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
       (prn "argument-info")
       (let [dbconn (make-database-connection db "guest" "")]
         (with-db dbconn
           (let [arg (read-argument id)
                 arg (pack-argument arg)
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
               {:status 200             ; 200 is OK
                :headers {"Content-Type" "application/xml"}
                :body xml})))))

  ;; SVG Maps

  (ANY "/map/:db" {params :params}
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
  (GET "/theory" []
       ;; TODO
       (json-response [walton-schemes]))

  (GET "/theory/:id" []
       ;; TODO
       (json-response walton-schemes))

  (GET "/scheme" []                     ; return all schemes
       (json-response (vals schemes-by-id)))
      
  (GET "/scheme/:id" [id]  ;; return the scheme with the given id
       (json-response (get schemes-by-id (symbol id))))
      
  (POST "/matching-schemes" request ; return all schemes with conclusions matching a goal
        (let [goal (unpack-statement (json/read-json (slurp (:body request))))]
          (json-response (get-schemes schemes-by-predicate goal {} true))))
      
  (POST "/apply-scheme/:db/:id" request 
	;; apply the scheme with the given id to the substitutions in the body
	;; and add the resulting arguments, if they are ground, to the 
	;; database. Returns a list of the ids of the new arguments.
        (let [data (json/read-json (slurp (:body request)))
              subs (unpack-subs (:subs data))
              attributes (unpack-arg-attrs (:attributes data))
              scheme (get schemes-by-id (symbol (:id (:params request))))]
          (let [responses (instantiate-scheme scheme subs)
                [username password] (get-username-and-password request)
                dbconn (make-database-connection (:db (:params request)) username password)]
            (prn "attributes: " attributes)
            (with-db dbconn
              (json-response 
               (reduce (fn [ids response]
                         (assume (:assumptions response))
                         (if (seq ids)
                           (conj ids (create-argument (:argument response)))
                           ;; the first argument is the main one. It is
                           ;; created with the attributes sent to the service
                           (conj ids (create-argument (merge
                                                       (:argument response)
                                                       attributes)))))
                       []
                       responses))))))

  (POST "/apply-substitutions" request
	;; apply the given substitutions to the given statement
        ;; and returns the result
        (let [content (json/read-json (slurp (:body request)))
              subs (unpack-subs (:substitutions content))
              statement (unpack-statement (:statement content))]
          (prn subs)
          (prn statement)
          (json-response (apply-substitutions subs statement))))

  ;; Policies
      
  (GET "/policies" []
       (json-response policies))

  (GET "/evaluate-policy/:db/:policykey/:qid/:policyid" [db policykey qid policyid]
       (let [dbconn (make-database-connection db "guest" "")]
         (with-db dbconn
           (let [ag (export-to-argument-graph dbconn)
                 ag (evaluate-policy (symbol qid) (symbol policyid)
                                     (policies (symbol policykey)) ag)
                 root "root"
                 passwd "pw1"
                 dbname (str "db-" (make-uuid))
                 dbconn2 (make-database-connection dbname root passwd)
                 metadata (map map->metadata (list-metadata))]
             (create-argument-database dbname root passwd (first metadata))
             (import-from-argument-graph dbconn2 ag false)
             (with-db dbconn2
               (doseq [m (rest metadata)]
                 (create-metadata m)))
             (json-response {:db dbname})))))

  ;; Argument Evaluation
    
  (POST "/evaluate-argument-graph/:db" request
       (let [[username password] (get-username-and-password request)
             db (make-database-connection (:db (:params request)) username password)]
         (with-db db
           (let [ag1 (export-to-argument-graph db)
                 ag2 (evaluate aspic-grounded ag1)]
             (doseq [sn (vals (:statement-nodes ag2))]
               (update-statement (str (:id sn))
                                 {:value (:value sn)}))
             (doseq [an (vals (:argument-nodes ag2))]
               (update-argument (str (:id an))
                                {:value (:value an)}))
             (json-response true)))))

  ;; Other 
      
  (GET "/" [] 
       "<h1>Carneades Web Service</h1>
            <p>This web service is part of the <a href=\"http://carneades.github.com\">
            Carneades Argumentation System</a></p>")                                                                
  (route/resources "/")
  (route/not-found "Page not found.")

  )

(def carneades-web-service
  (handler/site carneades-web-service-routes))

(defn get-request [resource web-app & params]
  (web-app {:request-method :get :uri resource :params (first params)}))

(defn post-request [resource web-app headers body & params]
  (web-app {:request-method :post :uri resource :headers headers :body (char-array body) :params (first params)}))

(defn put-request [resource web-app headers body & params]
  (web-app {:request-method :put :uri resource :headers headers :body (char-array body) :params (first params)}))

(defn delete-request [resource web-app headers body & params]
  (web-app {:request-method :delete :uri resource :headers headers :body (char-array body) :params (first params)}))

