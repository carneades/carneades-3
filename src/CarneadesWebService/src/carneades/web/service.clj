;;; Copyright (c) 2011 Fraunhofer Gesellschaft
;;; Licensed under the EUPL V.1.1

(ns carneades.web.service
   (:use clojure.data.json
         clojure.pprint
         compojure.core
         carneades.engine.statement
         carneades.database.db)
  (:require [compojure.route :as route]
            [compojure.handler :as handler]
            [clojure.java.jdbc :as jdbc]))

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

(defn- unpack-statement
  [m]
  (assoc (map->statement m) 
         :atom (read-string (:atom m))))

(defn- pack-argument
  [arg]
  (if (nil? arg)
    nil
    (merge arg
           {:conclusion (pack-statement (:conclusion arg)),
            :premises (map (fn [p] (assoc p :statement  (pack-statement (:statement p))))
                           (:premises arg))})))

;; To Do (CRITICAL): handle database exceptions to avoid printing of database password
;; To Do: wrap the delete and other destructive operations in transactions.
;; To Do: commands for logging into and creating databases
;; To Do: make all read operations public, without login, at least optionally
;; To Do: retrieving passwords from cookies
;; To Do: include the ids of undercutters and rebuttals in argument records
;; To Do: search operations
;; To Do: LKIF import/export
;; To Do: OPML export
;; To Do: review security issues and validate input
  
(defroutes handlers
  ;; Metadata         
  (GET "/metadata/:db" [db] 
       (json-response (list-metadata (make-db db "pw1"))))
  (GET "/metadata/:db/:id" [db id] 
       (json-response (read-metadata (make-db db "pw1") (read-string id))))
  (POST "/metadata" {params :params}  (json-response "Not Yet Implemented")) 
  (PUT "/metadata" {params :params}  (json-response "Not Yet Implemented"))  
  (DELETE "/metadata/:db/:id" [db id] 
          (json-response (delete-metadata (make-db db "pw1") (read-string id))))
                 
  ;; Statements
  (GET "/statement/:db" [db] 
       (json-response (map pack-statement (list-statements (make-db db "pw1")))))
  (GET "/statement/:db/:id" [db id] 
       (json-response (pack-statement (read-statement (make-db db "pw1") 
                                                      (read-string id)))))
           
  (POST "/statement/:db" request  
        (let [m (read-json (slurp (:body request)))
              s (unpack-statement m)]
          (let [db (make-db (:db (:params request)) "pw1")]
            (jdbc/with-connection 
              db
              (jdbc/transaction
                (json-response (create-statement db s)))))))

  (PUT "/statement" {params :params}  (json-response "Not Yet Implemented"))  
  (DELETE "/statement/:db/:id" [db id] 
          (json-response (delete-statement (make-db db "pw1") (read-string id))))
                    
  ;; Arguments     
  (GET "/argument/:db" [db] 
       (json-response (map pack-argument (list-arguments (make-db db "pw1")))))
  (GET "/argument/:db/:id" [db id] 
       (json-response (pack-argument (read-argument (make-db db "pw1") (read-string id)))))
  (POST "/argument" {params :params}  (json-response "Not Yet Implemented"))
  (PUT "/argument" {params :params}  (json-response "Not Yet Implemented"))  
  (DELETE "/argument/:db/:id" [db id] 
          (json-response (delete-argument (make-db db "pw1") (read-string id))))
           
  ;; Other         
  (GET "/" [] "<h1>Carneades Web Service</h1>")
  (route/resources "/")
  (route/not-found "Page not found"))

(def carneades-web-service
  (handler/site handlers))
