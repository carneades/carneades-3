;;; Copyright (c) 2011 Fraunhofer Gesellschaft
;;; Licensed under the EUPL V.1.1

(ns ^{:doc  "A database schema and CRUD functions for managing debate databases."}
       carneades.database.admin
  (:use clojure.pprint
        carneades.engine.uuid
        carneades.engine.dublin-core)
  (:require
        [carneades.database.db :as db]
        [clojure.java.jdbc :as jdbc]))


(defn create-debate-database
  "Initialize the debate database by creating the tables. 
   Returns true if the database is successul created and initialized"
  [db-name root-username root-password]
  (let [db  (db/make-database-connection db-name root-username root-password)]
    (jdbc/with-connection 
      db
      (jdbc/transaction
        
        (jdbc/create-table 
          :debate
          [:id "varchar primary key not null"]  ; URN 
          [:created "varchar"]   ; date; http://www.w3.org/TR/NOTE-datetime     
          [:title "varchar"]                       
          [:public "boolean not null"]) ; true if published
        
        ;; Grant read access to the public role
        ;; and create a guest user account with this role.
        (jdbc/do-commands "grant select on debate to public"
                          "create user guest password ''"
                          "grant public to guest")
        
        true))))


; Debates

(defn create-debate
  "map -> urn
   Given a {:title ..., :password ...} map, creates a debate 
   record and inserts it into the admin database.  
   Also creates an argument database for the debate,
   protected by the password. Returns the id (URN) of 
   the new database."
  [m]   
  {:pre [(map? m)]}
  (let [id (make-urn)]
    (jdbc/insert-record :debate (assoc m :id id))
    (db/create-argument-database 
      id 
      "root" 
      (:password m) 
      (make-metadata :title (:title m)))
    id))

(defn read-debate 
  "urn -> map or nil
   Retrieves the debate record with the given id from the database.
   Returns nil if no debate with the given id exists in the
   database."
  [id]
  (jdbc/with-query-results 
    res1 ["SELECT * FROM debate WHERE id=?" id]
    (if (empty? res1) 
      nil 
      (dissoc (first res1) :id))))

(defn list-debates
  "Returns a sequence of all the debate records in the database"
  []
  (let [ids (jdbc/with-query-results 
              res1 ["SELECT id FROM debate"]
              (doall (map :id res1)))]
    (doall (map (fn [id] (read-debate id)) ids))))

(defn update-debate
  "urn map -> boolean
   Updates the debate record with the given id in the database with the values
   in the map.  Returns true if the update was successful." 
  [id m]
  {:pre [(urn? id) (map? m)]}
  (condp = (first (jdbc/update-values :debate ["id=?" id] m))
    0 false,
    1 true))

; To do: do not export delete-debate until it has been extended
; to move its database and other files to a "trash can" or archive, where they
; can be recovered.

(defn- delete-debate
  "Deletes a debate entry with the given the id. Returns true."
  [id]
  {:pre [(urn? id)]}
  (jdbc/delete-rows :debate ["id=?" id])
  true)


