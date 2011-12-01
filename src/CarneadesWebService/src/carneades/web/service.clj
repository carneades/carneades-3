;;; Copyright (c) 2011 Fraunhofer Gesellschaft
;;; Licensed under the EUPL V.1.1

(ns carneades.web.service
   (:use clojure.data.json
         clojure.pprint
         compojure.core
         carneades.engine.statement
         carneades.engine.argument
         carneades.engine.dublin-core
         carneades.database.db)
  (:require [compojure.route :as route]
            [compojure.handler :as handler]
            [clojure.java.jdbc :as jdbc]))

;; To Do: 
;; - commands for logging into and creating databases
;; - retrieving user name and password from the request 
;; - include the ids of undercutters and rebuttals in argument records
;; - search operations, including full text search
;; - LKIF import/export
;; - OPML export
;; - review security issues 
;; - validate input?
  

;(defmacro with-db [db & body]   
;  `(try (jdbc/with-connection 
;           ~db
;           (jdbc/transaction ~@body))
;        (catch Exception t# 
;               {:status 500
;                 :body "The server encountered an unexpected condition which prevented it from fulfilling the request."})))
;


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
        (statement? stmt) (merge stmt {:atom (str (:atom stmt))}),
        :else nil))

(defn- unpack-statement [s]
   (cond (string? s) (read-string s),
         (map? s) (assoc (map->statement s) 
                    :atom (read-string (:atom s))),
        :else nil))

(defn- pack-argument
  [arg]
  (if (nil? arg)
    nil
    (merge arg
           {:conclusion (pack-statement (:conclusion arg)),
            :premises (map (fn [p] (assoc p :statement 
                                          (pack-statement (:statement p))))
                           (:premises arg))})))

(defn- unpack-argument [arg]
  (assoc arg
         :conclusion (unpack-statement (:conclusion arg))
         :premises (map (fn [p] (assoc p :statement (unpack-statement (:statement p))))
                        (:premises arg))))

;; We don't use the defroutes macro.
;; This allow handlers to be reused in another project
(def carneades-web-service-routes
     
     ;; Metadata         
     [ 
      (GET "/metadata/:db" [db] 
           (let [db2 (make-database-connection db "guest" "")]
             (with-db db2 (json-response (list-metadata db2)))))
      
      (GET "/metadata/:db/:id" [db id]
           (let [db2 (make-database-connection db "guest" "")]
             (with-db db2 (json-response (read-metadata db2 (read-string id))))))
      
      (POST "/metadata" request
            (let [m (read-json (slurp (:body request)))
                  db (make-database-connection (:db (:params request)) "root" "pw1")]
              (with-db db (json-response (create-metadata db (map->metadata db m)))))) 
      
      (PUT "/metadata" request   
           (let [m (read-json (slurp (:body request)))
                 db (make-database-connection (:db (:params request)) "root" "pw1")
                 id (read-string (:id (:params request)))]
             (with-db db (json-response (update-metadata db id m))))) 
      
      (DELETE "/metadata/:db/:id" [db id] 
              (let [db2 (make-database-connection db "root" "pw1")]
                (with-db db2 (json-response (delete-metadata db2 (read-string id))))))
      
      ;; Statements
      
      (GET "/statement/:db" [db] 
           (let [db2 (make-database-connection db "guest" "")]
             (with-db db2 (json-response (map pack-statement (list-statements db2))))))           
      
      (GET "/statement/:db/:id" [db id] 
           (let [db2 (make-database-connection db "guest" "")]
             (with-db db2 
               (let [res (json-response (pack-statement (read-statement db2 (read-string id))))]
                 (pprint res)
                 (prn " ressss ")
                 res
                 ))))
      
      (POST "/statement/:db" request  
            (let [m (read-json (slurp (:body request)))
                  s (unpack-statement m)
                  db (make-database-connection (:db (:params request)) "root" "pw1")]
              (prn request)
              (with-db db (json-response (create-statement db (map->statement s))))))
      
      (PUT "/statement" request  
           (let [m (read-json (slurp (:body request)))
                 s (unpack-statement m)
                 db (make-database-connection (:db (:params request)) "root" "pw1")
                 id (read-string (:id (:params request)))]
             (with-db db (json-response (update-statement db id s)))))        
      
      (DELETE "/statement/:db/:id" [db id] 
              (let [db2 (make-database-connection db "root" "pw1")]
                (with-db db2 (json-response (delete-statement db2 (read-string id))))))
      
      ;; Arguments  
      
      (GET "/argument/:db" [db]
           (let [db2 (make-database-connection db "guest" "")] 
             (with-db db2 (json-response (map pack-argument (list-arguments db2))))))
      
      (GET "/argument/:db/:id" [db id]
           (let [db2 (make-database-connection db "guest" "")]  
             (with-db db2 (json-response (pack-argument (read-argument db2 (read-string id)))))))
      
      (POST "/argument" request  
            (let [m (read-json (slurp (:body request)))
                  arg (unpack-argument m)
                  db (make-database-connection (:db (:params request)) "root" "pw1")]
              (with-db db (json-response (create-argument db (map->argument arg))))))
      
      (PUT "/argument" request  
           (let [m (read-json (slurp (:body request)))
                 arg (unpack-argument m)
                 db (make-database-connection (:db (:params request)) "root" "pw1")
                 id (read-string (:id (:params request)))]
             (with-db db (json-response (update-argument db id arg)))))   
      
      (DELETE "/argument/:db/:id" [db id] 
              (let [db2 (make-database-connection db "root" "pw1")]
                (with-db db2
                  (json-response (delete-argument db2 (read-string id))))))
      
      ;; Namespaces
      
      (GET "/namespace/:db" [db]
           (let [db2 (make-database-connection db "guest" "")] 
             (with-db db2 (json-response (list-namespaces db2)))))
      
      (GET "/namespace/:db/:prefix" [db prefix]
           (let [db2 (make-database-connection db "guest" "")]  
             (with-db db2 (json-response (read-namespace db2 prefix)))))

      (POST "/namespace/" request  
            (let [prefix (:prefix (:params request)),
                  uri (read-json (slurp (:body request))),
                  db (make-database-connection (:db (:params request)) "root" "pw1")]
              (with-db db (json-response (create-namespace db prefix uri)))))
      
      (PUT "/namespace/" request  
           (let [prefix (:prefix (:params request)),
                 uri (read-json (slurp (:body request))),
                 db (make-database-connection (:db (:params request)) "root" "pw1")]
             (with-db db (json-response (update-namespace db prefix uri)))))
      
      (DELETE "/namespace/:db/:prefix" [db prefix] 
              (let [db2 (make-database-connection db "root" "pw1")]
                (with-db db2
                  (json-response (delete-namespace db2 prefix)))))
      
      ;; Statement Polls
      
      (GET "/statement-poll/:db" [db]
           (let [db2 (make-database-connection db "guest" "")] 
             (with-db db2 (json-response (list-statement-poll db2)))))
      
      (GET "/statement-poll/:db/:id" [db id]
           (let [db2 (make-database-connection db "guest" "")]  
             (with-db db2 (json-response (read-statement-poll db2 (read-string id))))))

      (POST "/statement-poll" request  
            (let [userid (:id (:params request)),
                  votes (read-json (slurp (:body request))),
                  db (make-database-connection (:db (:params request)) "root" "pw1")]
              (with-db db (json-response (create-statement-poll db userid votes)))))
      
      (PUT "/statement-poll" request  
           (let [userid (:id (:params request)),
                 votes (read-json (slurp (:body request)))
                 db (make-database-connection (:db (:params request)) "root" "pw1")]
             (with-db db (json-response (update-statement-poll db userid votes)))))
      
      (DELETE "/statement-poll/:db/:id" [db id] 
              (let [db2 (make-database-connection db "root" "pw1")]
                (with-db db2
                  (json-response (delete-statement-poll db2 (read-string id)))))) 
      
      ;; Argument Polls
      
      (GET "/argument-poll/:db" [db]
           (let [db2 (make-database-connection db "guest" "")] 
             (with-db db2 (json-response (list-argument-poll db2)))))
      
      (GET "/argument-poll/:db/:id" [db id]
           (let [db2 (make-database-connection db "guest" "")]  
             (with-db db2 (json-response (read-argument-poll db2 (read-string id))))))

      (POST "/argument-poll" request  
            (let [userid (:id (:params request)),
                  votes (read-json (slurp (:body request))),
                  db (make-database-connection (:db (:params request)) "root" "pw1")]
              (with-db db (json-response (create-argument-poll db userid votes)))))
      
      (PUT "/argument-poll" request  
           (let [userid (:id (:params request)),
                 votes (read-json (slurp (:body request)))
                 db (make-database-connection (:db (:params request)) "root" "pw1")]
             (with-db db (json-response (update-argument-poll db userid votes)))))
      
      (DELETE "/argument-poll/:db/:id" [db id] 
              (let [db2 (make-database-connection db "root" "pw1")]
                (with-db db2
                  (json-response (delete-argument-poll db2 (read-string id)))))) 
      
      ;; Other 
      
      (GET "/" [] 
           "<h1>Carneades Web Service</h1>
     <p>This web service is part of the <a href=\"http://carneades.github.com\">
     Carneades Argumentation System</a></p>")                                                                      
      (route/resources "/")
      (route/not-found "Page not found.")]) 

(def carneades-web-service
  (handler/site (apply routes carneades-web-service-routes)))
