(ns carneades.database.db
  (:use clojure.pprint
        carneades.engine.statement
        carneades.engine.argument)
  (:require
        [clojure.java.jdbc :as jdbc]
        [clojureql.core :as cql]))

;;; Databases

(defn make-db  
  "Returns a map describing a database with the given name and password."
  [db-name passwd]
  (let [db-protocol "file"             ; "file|mem|tcp"
        db-host     "/Users/tgo/Test"] ; "path|host:port" 
    ;;; TO DO ? remove path dependency above
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
  (jdbc/with-connection 
    db
    
    (jdbc/create-table 
      :string
      [:id "int identity"]
      [:en "varchar not null"]   ; English
      [:nl "varchar"]            ; Dutch          
      [:fr "varchar"]            ; French
      [:de "varchar"]            ; German
      [:it "varchar"]            ; Italian
      [:sp "varchar"])           ; Spanish
    ; and so on for other langauges
    
    (jdbc/create-table 
      :metadata
      [:id "int identity"]
      [:contributor "varchar"]
      [:coverage "varchar"]
      [:creator "varchar"]
      [:date "varchar"]       ; http://www.w3.org/TR/NOTE-datetime                         
      [:description "int"]
      [:format "varchar"]     ; A list of MIME types, comma separated
      [:identifier "varchar"] 
      [:language "varchar"]
      [:publisher "varchar"]
      [:relation "varchar"]
      [:rights "varchar"]
      [:source "varchar"]
      [:subject "varchar"]
      [:title "varchar"]
      [:type "varchar"]       ; see: http://dublincore.org/documents/dcmi-type-vocabulary/
      ["foreign key(description) references string(id)"])
    
    (jdbc/create-table 
      :statement 
      [:id "int identity"]
      [:weight "double default 0.50"]
      [:value "double default 0.50"]
      [:standard "tinyint default 0"]   ; 0=pe, 1=cce, 2=brd, 3=dv 
      [:atom "varchar"]                 ; Clojure s-expression
      [:text "int"]
      [:main "boolean default false"]   ; true if a main issue
      [:header "int"]
      ["foreign key(text) references string(id)"]
      ["foreign key(header) references metadata(id)"])
    
    (jdbc/create-table 
      :argument
      [:id "int identity"]
      [:conclusion "int not null"]
      [:weight "double default 0.50"]
      [:value "double default 0.50"]
      [:scheme "varchar"]                  ; URI of the scheme
      [:direction "boolean default true"]  ; true=pro, false=con
      [:header "int not null"]
      ["foreign key(conclusion) references statement(id)"]
      ["foreign key(header) references metadata(id)"])
    
    (jdbc/create-table 
      :premise
      [:id "int identity"]
      [:argument "int not null"]
      [:statement "int not null"]
      [:polarity "boolean default true"]    ; true=positive, false=negative
      [:role "varchar"]
      ["foreign key(argument) references argument(id)"]
      ["foreign key(statement) references statement(id)"])                         
    
    (jdbc/create-table 
      :namespace
      [:prefix "varchar not null"]
      [:uri    "varchar not null"])
    
    (jdbc/create-table 
      :stmtpoll         ; statement poll
      [:userid "varchar not null"]   
      [:statement "int not null"]
      [:opinion "double default 0.5"]
      ["foreign key(statement) references statement(id)"])
    
    (jdbc/create-table 
      :argpoll         ; argument poll
      [:userid "varchar not null"]   
      [:argument "int not null"]
      [:opinion "double default 0.5"]
      ["foreign key(argument) references argument(id)"])))

;;; Strings

(defn create-string 
  "Creates a string record, with translations in several languages,
   and inserts it into a database.  Returns the id of the new string."
  [db & key-values]   
  (jdbc/with-connection db
     (let [result (jdbc/insert-record 
                    :string
                    (apply hash-map key-values))]
       (first (vals result)))))

;;; Metadata

(defn create-metadata 
  "Creates a metadata record and inserts it into a database.  
   Returns the id of the new metadata record."
  [db & key-values]   
  (jdbc/with-connection db
     (let [result (jdbc/insert-record 
                    :metadata
                    (apply hash-map key-values))]
       (first (vals result)))))

;;; Statements

(defn- standard->int 
  [standard]
  (condp = 
    :pe 0,
    :cce 1,
    :brd 2,
    :dv 3))
  
(defn create-statement 
  "Creates and inserts a statement record in the database for the atom of the given
   literal. Returns the id of the new statement record."
  [db literal]   
  {:pre [(literal? literal)]}
  (jdbc/with-connection db
     (cond (sliteral? literal) 
              (first (vals (jdbc/insert-record
                             :statement {:atom (literal-atom literal)})))
           (statement? literal)
              (if (and (not (nil? (:text literal)))
                       (not (empty? (:text literal))))
                (let [str-id (first (vals (jdbc/insert-record
                                        :string
                                        (:text literal))))]
                  (first (vals (jdbc/insert-record
                                 :statement {:atom (str (:atom literal)),
                                             :weight (:weight literal),
                                             :standard (standard->int (:standard literal)),
                                             :text str-id}))))))))

(defn read-statement
  "database int -> statement
   Retreives the statement with the give id from the database."
  
  ; START HERE
  )
  
                                         
;(defn create-argument 
;  "Creates a one-step argument and inserts it into a database.  Returns
;   the id of the new argument."
;  [db & key-values]   
;  (jdbc/with-connection db
;     (let [result (jdbc/insert-records :statement
;                       (merge {:weight 0.5
;                               :value 0.5
;                               :standard 0  ; pe
;                               :atom ""
;                               :text nil
;                               :main false
;                               :header nil}
;                              (apply hash-map key-values)))]
;       (first (vals (first result))))))


;;; Arguments

(defn test-query
  [db table]
  (jdbc/with-connection 
    db
    (jdbc/with-query-results 
      res [(str "SELECT * FROM " table)]
      (doall res))))


