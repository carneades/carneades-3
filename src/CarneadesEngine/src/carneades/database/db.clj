(ns carneades.database.db
  (:use clojure.pprint
        korma.db))

(defdb mydb {:classname "org.h2.Driver" 
             :subprotocol "h2"
             :subname "tcp://localhost:9092/Sample"
             :user     "korma"
             :password "kormapass"})

(defn make-db  
  "Returns a map describing a database with the given name and password."
  [db-name passwd]
  (let [db-protocol "tcp"             ; "file|mem|tcp"
        db-host     "localhost:9092"] ; "path|host:port"
    {:classname   "org.h2.Driver" 
     :subprotocol "h2"
     :subname (str db-protocol "://" db-host "/" db-name)
     ; Any additional keys are passed to the driver
     ; as driver-specific properties.
     :user     "root"
     :password passwd}))

(defn- init-db
  "Initialize the database by creating the tables."
  [db]
  
  (defentity users
             (entity-fields :first :last) ;; Default fields for selects
             (database db) 
             
             ;; mutations
             (prepare ..) ;; apply a function before storing in the db
             (transform ..) ;; apply a function to all select results
             
             ;;Relationships
             (has-one email) 
             ;; assumes users.id = email.users_id
             (has-many address) 
             ;; assumes users.id = address.users_id
             ;; but gets the results in a second query
             ;; for each element
             (belongs-to account)
             ;; assumes users.account_id = account.id
             (has-one email {:fk :emailID}))
  ;; you can optionally specify the foreign key
  )

