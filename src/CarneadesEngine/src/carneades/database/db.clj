(ns carneades.database.db
  (:use clojure.pprint
        carneades.engine.dublin-core
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
        db-host     "/Library/Application Support/Carneades/Databases"] ; "path|host:port" 
    ;;; TO DO ? remove path and system dependency above
    ;;; Use a properties file or parameters
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
      :stringmap
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
      [:format "varchar"]     ; A list of MIME types, semicolon separated
      [:identifier "varchar"] 
      [:language "varchar"]
      [:publisher "varchar"]
      [:relation "varchar"]
      [:rights "varchar"]
      [:source "varchar"]
      [:subject "varchar"]
      [:title "varchar"]
      [:type "varchar"]       ; see: http://dublincore.org/documents/dcmi-type-vocabulary/
      ["foreign key(description) references stringmap(id)"])
    
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
      ["foreign key(text) references stringmap(id)"]
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

;; To Do: function to delete a database.  Perhaps this should
;; be private and called to clean up after exporting a database to CAF XML,
;; to help avoid deleting data without first creating a backup.

;;; Strings

(defn create-stringmap
  "database map -> integer
   Creates a stringmap record, with translations in several languages,
   and inserts it into a database.  Returns the id of the new stringmap."
  [db m]   
  {:pre [(map? m)]}
  (jdbc/with-connection db
     (let [result (jdbc/insert-record :stringmap m)]
       (first (vals result)))))

(defn read-stringmap 
  "database integer -> map or nil
   Retrieves the string map with the given id from the database.
   Returns nil if not stringmap with the given id exists in the
   database."
  [db id]
  (jdbc/with-connection 
    db
    (jdbc/with-query-results 
      res1 [(str "SELECT * FROM stringmap WHERE id='" id "'")]
      (if (empty? res1) nil (dissoc (first res1) :id)))))
  
(defn update-stringmap 
  "database integer map -> boolean
   Updates the stringmap with the given id in the database
   with the values in the map. Returns true if the update
   succeeds."
  [db id m]
  {:pre [(integer? id) (map? m)]}
  (jdbc/with-connection
    db
    (condp = (first (jdbc/update-values :stringmap ["id=?" id] m))
      0 false
      1 true)))

(defn delete-stringmap 
  "Deletes a stringmap entry with the given the id"
  [db id]
  (jdbc/with-connection db
    (jdbc/delete-rows :stringmap ["id=?" id])))

;;; Metadata

(defn create-metadata 
  "Inserts a metadata structure into a database.  
   Returns the id of the record in the database."
  [db metadata]
  {:pre [(metadata? metadata)]}
  (jdbc/with-connection 
    db
    (let [str-id (if (:description metadata)
                   (first (vals (jdbc/insert-record
                                  :stringmap
                                  (:description metadata)))))]
      (first (vals (jdbc/insert-record 
                     :metadata
                     (if str-id
                       (assoc metadata :description str-id)
                       metadata)))))))

(defn read-metadata
  "Retrieves the metadata with the given id from the database, 
   and returns it as a metadata record. Returns nil if there
   is no metadata record in the database with this id."
  [db id]
  (jdbc/with-connection 
    db
    (let [md (jdbc/with-query-results 
               res1 [(str "SELECT * FROM metadata WHERE id='" id "'")]
               (if (empty? res1) nil (dissoc (first res1) :id)))]
      (if (:description md)
        (let [d (jdbc/with-query-results 
                  res2 [(str "SELECT * FROM stringmap WHERE id='" (:description md) "'")]
                  (if (empty? res2) nil (dissoc (first res2) :id)))]
          (if d
            (doall (merge (merge (make-metadata) md)
                          {:description d}))
            (doall (merge (make-metadata) md))))))))  

(defn update-metadata
  "database integer map -> boolean
   Updates the metadata record with the given in in the database with the values
   in the map.  Returns true if the update was successful." 
  [db id md]
  {:pre [(integer? id) (map? md)]}
  (jdbc/with-connection 
    db
      (let [description-id1 (if (:description md)
                              (jdbc/with-query-results 
                              res [(str "SELECT description FROM metadata WHERE id='" id "'")]
                              (if (empty? res) nil (:description (first res)))))
            description-id2  (if description-id1 
                               (do (update-stringmap db description-id1 (:description md))
                                   description-id1)
                               (if (:description md) (create-stringmap db (:description md))))]
        (condp = (first (jdbc/update-values
                          :metadata
                          ["id=?" id]
                          (merge md {:description description-id2})))
          0 false,
          1 true))))

(defn delete-metadata
  "Deletes a metadata entry with the given the id. Returns true."
  [db id]
  {:pre [(integer? id)]}
  (jdbc/with-connection 
    db 
    (let [str-id (jdbc/with-query-results 
                   res [(str "SELECT description FROM metadata WHERE id='" id "'")]
                   (if (empty? res) nil (:description (first res))))]                   
      (jdbc/delete-rows :metadata ["id=?" id])
      (if str-id (jdbc/delete-rows :stringmap ["id=?" str-id]))))
      true)
  
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
  (jdbc/with-connection 
    db
    (cond (sliteral? literal) 
          (first (vals (jdbc/insert-record
                         :statement {:atom (literal-atom literal)})))
          (statement? literal)
                (let [text-id (if (:text literal) (create-stringmap db (:text literal))),
                      header-id (if (:header literal) (create-metadata db (:header literal)))]
                  (first (vals (jdbc/insert-record
                                 :statement {:atom (str (:atom literal)),
                                             :header header-id,
                                             :weight (:weight literal),
                                             :main (:main literal),
                                             :standard (standard->int (:standard literal)),
                                             :text text-id})))))))


(defn read-statement
  "database int -> statement or nil
   Retreives the statement with the give id from the database.
   Returns nil if there is not statement with this id."
  [db id]
  {:pre [(integer? id)]}
    (jdbc/with-connection 
      db
      (let [s (jdbc/with-query-results 
                   res [(str "SELECT * FROM statement WHERE id='" id "'")]
                   (if (empty? res) nil (first res)))
            h (if (:header s) (jdbc/with-query-results
                      res [(str "SELECT * FROM metadata WHERE id='" (:header s) "'")]
                   (if (empty? res) nil (first res))))
            t (if (:text s) (jdbc/with-query-results
                      res [(str "SELECT * FROM stringmap WHERE id='" (:text s) "'")]
                   (if (empty? res) nil (first res))))]
        (if s 
          (-> (make-statement)
              (merge (dissoc s :id))
              (merge {:atom (read-string (:atom s))})
              (merge {:header (dissoc h :id), 
                      :text (dissoc t :id)}))))))
 
(defn update-statement
  "database integer map -> boolean
   Updates the statement record with the given in in the database with the values
   in the map.  Returns true if the update was successful." 
  [db id m]
  {:pre [(integer? id) (map? m)]}
  (jdbc/with-connection 
    db
      (let [header-id1 (if (:header m)
                              (jdbc/with-query-results 
                              res [(str "SELECT header FROM statement WHERE id='" id "'")]
                              (if (empty? res) nil (:header (first res)))))
            header-id2  (if header-id1 
                               (do (update-metadata db header-id1 (:header m))
                                   header-id1)
                               (if (:header m) (create-metadata db (merge (make-metadata) (:header m)))))
            text-id1 (if (:text m)
                              (jdbc/with-query-results 
                              res [(str "SELECT text FROM statement WHERE id='" id "'")]
                              (if (empty? res) nil (:text (first res)))))
            text-id2  (if text-id1 
                               (do (update-stringmap db text-id1 (:text m))
                                   header-id1)
                               (if (:text m) (create-stringmap db (:text m))))]
        (condp = (first (jdbc/update-values
                          :statement
                          ["id=?" id]
                          (merge m {:header header-id2
                                    :text text-id2})))
          0 false,
          1 true))))
  

; START HERE
(defn delete-statement 
  "Deletes a statement entry with the given the id. Returns true."
  [db id]
  {:pre [(integer? id)]}
  (jdbc/with-connection 
    db 
    (let [text-id (jdbc/with-query-results 
                   res [(str "SELECT text FROM statement WHERE id='" id "'")]
                   (if (empty? res) nil (:text (first res))))
          text-id (jdbc/with-query-results 
                   res [(str "SELECT text FROM statement WHERE id='" id "'")]
                   (if (empty? res) nil (:text (first res))))]                   
      (jdbc/delete-rows :metadata ["id=?" id])
      (if text-id (delete-statementmap db text-id)))
      true)
                                         
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

(defn list-table
  [db table]
  (jdbc/with-connection 
    db
    (jdbc/with-query-results 
      res [(str "SELECT * FROM " table)]
      (doall res))))


