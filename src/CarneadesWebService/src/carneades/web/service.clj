;;; Copyright (c) 2011 Fraunhofer Gesellschaft
;;; Licensed under the EUPL V.1.1

(ns carneades.web.service
   (:use clojure.data.json
         compojure.core
         carneades.database.db)
  (:require [compojure.route :as route]
            [compojure.handler :as handler]))


(def db (make-db "db1" "pw1"))

(defn json-response [data & [status]]
  {:status (or status 200)
   :headers {"Content-Type" "application/json"}
   :body (json-str data)})

(defrecord Statement  ; statements are annotated literals
  [atom               ; atomic formula of propositional (symbol) or predicate logic (list)
   header             ; nil or dublin core metadata description of the model
   positive           ; boolean
   weight             ; nil or 0.0-1.0, default nil
   main               ; true if the statement is a main issue
   standard           ; proof-standard
   text])             ; (language -> string) map, natural language formulations of the statement

(defn- pack-statement 
  [stmt]
  (if (nil? stmt) 
    nil
    (merge stmt {:atom (str (:atom stmt))})))

(defn- pack-argument
  [arg]
  (if (nil? arg)
    nil
    (merge arg
           {:conclusion (pack-statement (:conclusion arg)),
            :premises (map (fn [p] (assoc p :statement  (pack-statement (:statement p))))
                           (:premises arg))})))
  
(defroutes handlers
  ;; Metadata         
  (GET "/metadata" [] (json-response "Not Yet Implemented"))
  (GET "/metadata/:id" [id] (json-response (read-metadata db (read-string id))))
  (POST "/metadata" {params :params}  (json-response "Not Yet Implemented")) 
  (PUT "/metadata" {params :params}  (json-response "Not Yet Implemented"))  
  (DELETE "/metadata/:id" [id] (json-response (delete-metadata db id)))
                 
  ;; Statements
  (GET "/statement" [] (json-response "Not Yet Implemented"))
  (GET "/statement/:id" [id] (json-response (pack-statement (read-statement db (read-string id)))))
  (POST "/statement" {params :params}  (json-response "Not Yet Implemented"))
  (PUT "/statement" {params :params}  (json-response "Not Yet Implemented"))  
  (DELETE "/statement/:id" [id] (json-response (delete-statement db id)))
                    
  ;; Arguments     
  (GET "/argument/:id" [id] (json-response (pack-argument (read-argument db id))))
  (GET "/argument" [] (json-response "Not Yet Implemented"))
  (POST "/argument" {params :params}  (json-response "Not Yet Implemented"))
  (PUT "/argument" {params :params}  (json-response "Not Yet Implemented"))  
  (DELETE "/argument/:id" [id] (json-response (delete-argument db id)))
           
  ;; Other         
  (GET "/" [] "<h1>Carneades Web Service</h1>")
  (route/resources "/")
  (route/not-found "Page not found"))

(def carneades-web-service
  (handler/site handlers))
