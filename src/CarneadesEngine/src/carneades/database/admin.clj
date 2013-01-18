;;; Copyright (c) 2011 Fraunhofer Gesellschaft
;;; Licensed under the EUPL V.1.1

(ns ^{:doc  "A database schema and CRUD functions for managing debate databases."}
       carneades.database.admin
  (:use clojure.pprint
        carneades.engine.uuid
        carneades.engine.dublin-core)
  (:require [carneades.database.db :as db]
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
         :poll
         [:id "int auto_increment primary key not null"]
         [:userid "varchar not null"]
         [:casedb "varchar not null"]
         [:mainissueatompredicate "varchar not null"]
         [:opinion "double"]
         ;; enforce that a user is voting only once for a case
         ["UNIQUE KEY (userid, casedb)"]
         )

        (jdbc/create-table 
          :debate
          [:id "varchar primary key not null"]  ; URN 
          [:created "varchar"]   ; date; http://www.w3.org/TR/NOTE-datetime     
          [:title "varchar"]                       
          [:public "boolean not null"] ;; true if published
          )

        (jdbc/create-table
         ;; links debate to their polls
         :vote
         [:debate "varchar not null"]
         [:poll "varchar not null"]
         ["foreign key(debate) references debate(id)"]
         ["foreign key(poll) references poll(id)"])

        (jdbc/create-table
         ;; links poll to policies
         :policy
         [:id "int auto_increment primary key not null"]
         [:poll "int not null"]
         [:policy "varchar not null"]
         ["foreign key(poll) references poll(id)"]
         )
        
        ;; Grant read access to the public role
        ;; and create a guest user account with this role.
        (jdbc/do-commands "grant select on debate, poll, vote, policy to public"
                          "create user guest password ''"
                          "grant public to guest")
        
        true))))


; Debates

(defn create-debate
  "map -> urn
   Given a {:title ...} map, creates a debate 
   record and inserts it into the admin database.  
   "
  [m]   
  {:pre [(map? m)]}
  (let [id (or (:id m) (make-urn))]
    (jdbc/insert-record :debate (assoc m :id id))
    id))

(defn create-debate-with-database
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
  (jdbc/with-query-results 
    res1 ["SELECT * FROM debate"]
    (doall res1)))

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

(defn list-polls
  "Returns a sequence of all the polls in the database for the given debate."
  [id]
  (jdbc/with-query-results 
    res [(str "SELECT id, userid, mainissueatompredicate, opinion FROM poll "
              "INNER JOIN vote ON poll.id = vote.poll AND vote.debate = ?") id]
    (or (doall res) ())))

(defn create-poll
  "Adds a poll for the debate table and returns its id."
  [debateid poll policies]
  (let [id (first (vals (jdbc/insert-record :poll poll)))]
    (jdbc/insert-record :vote {:debate debateid
                               :poll id})
    (doseq [policy policies]
      (jdbc/insert-record :policy {:poll id
                                   :policy policy}))
    id))

(defn read-poll
  "Retrieves the poll having the given id."
  [pollid]
  (jdbc/with-query-results
    res ["SELECT * FROM poll WHERE id = ?" pollid]
    (first res)))

(defn get-opinions-for-case
  "Retrieves all opinions for the given case."
  [debateid casedb]
  (jdbc/with-query-results
    res [(str "SELECT opinion FROM poll "
              "INNER JOIN vote ON poll.id = vote.poll AND vote.debate = ? "
              "AND casedb = ?")
         debateid casedb]
    (doall (map :opinion res))))

(defn get-policies-for-debate
  "Retrieves all matched policies for the given debate."
  [debateid]
  (jdbc/with-query-results
    res [(str "SELECT policy FROM policy "
              "INNER JOIN vote, poll WHERE policy.poll = poll.id AND poll.id = vote.poll "
              "AND vote.debate = ?")
         debateid]
    (doall (map :policy res))))

(defn count-polls-for-debate
  "Returns the number of vote for the debate"
  [debateid]
  (jdbc/with-query-results
    res [(str "SELECT count(*) FROM poll "
              "INNER JOIN vote WHERE poll.id = vote.poll AND vote.debate = ?")
         debateid]
    (first (vals (first res)))))

(defn update-poll
  "Updates a poll with the values of the m map.
Returns true if the update was successful."
  [pollid m]
  (= (first (jdbc/update-values :poll ["id = ?" pollid] m)) 1))


