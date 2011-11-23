;;; Copyright (c) 2011 Fraunhofer Gesellschaft
;;; Licensed under the EUPL V.1.1

(ns carneades.database.db
  (:use clojure.pprint
        carneades.engine.dublin-core
        carneades.engine.statement
        carneades.engine.argument)
  (:require
        [clojure.java.jdbc :as jdbc]))

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
      :translation
      [:id "int identity"]
      [:en "varchar"]            ; English
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
      ["foreign key(description) references translation(id)"])
    
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
      ["foreign key(text) references translation(id)"]
      ["foreign key(header) references metadata(id)"])
    
    (jdbc/create-table 
      :argument
      [:id "int identity"]
      [:conclusion "int not null"]
      [:strict "boolean default false"]
      [:weight "double default 0.50"]
      [:value "double"]                    ; null means not evaluated
      [:scheme "varchar"]                  ; URI of the scheme
      [:pro "boolean default true"]        ; con argument if false
      [:header "int"]
      ["foreign key(conclusion) references statement(id)"]
      ["foreign key(header) references metadata(id)"])
    
    (jdbc/create-table 
      :premise
      [:id "int identity"]
      [:argument "int not null"]  
      [:statement "int not null"]
      [:positive "boolean default true"] 
      [:role "varchar"]
      [:implicit "boolean default false"]
      ["foreign key(argument) references argument(id)"]
      ["foreign key(statement) references statement(id)"])                         
    
    (jdbc/create-table 
      :namespace
      [:prefix "varchar primary key not null"]
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

;;; Translations

(defn create-translation
  "database map -> integer
   Creates a translation record and inserts it into a database.  
   Returns the id of the new translation."
  [db m]   
  {:pre [(map? m)]}
  (jdbc/with-connection db
     (let [result (jdbc/insert-record :translation m)]
       (first (vals result)))))

(defn read-translation 
  "database integer -> map or nil
   Retrieves the translation with the given id from the database.
   Returns nil if no translation with the given id exists in the
   database."
  [db id]
  (jdbc/with-connection 
    db
    (jdbc/with-query-results 
      res1 [(str "SELECT * FROM translation WHERE id='" id "'")]
      (if (empty? res1) nil (first res1)))))
  
(defn update-translation 
  "database integer map -> boolean
   Updates the translation with the given id in the database
   with the values in the map. Returns true if the update
   succeeds."
  [db id m]
  {:pre [(integer? id) (map? m)]}
  (jdbc/with-connection
    db
    (condp = (first (jdbc/update-values :translation ["id=?" id] m))
      0 false
      1 true)))

(defn delete-translation 
  "Deletes a translation with the given the id"
  [db id]
  (jdbc/with-connection db
    (jdbc/delete-rows :translation ["id=?" id])))

;;; Metadata

(defn create-metadata 
  "Inserts a metadata structure into a database.  
   Returns the id of the record in the database."
  [db metadata]
  {:pre [(metadata? metadata)]}
  (jdbc/with-connection 
    db
    (let [str-id (if (:description metadata)
                   (create-translation db (:description metadata)))]
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
               (if (empty? res1) nil (first res1)))]
      (if (:description md)
        (let [d (jdbc/with-query-results 
                  res2 [(str "SELECT * FROM translation WHERE id='" (:description md) "'")]
                  (if (empty? res2) nil (first res2)))]
          (if d
            (doall (merge (merge (make-metadata) md)
                          {:description d}))
            (doall (merge (make-metadata) md))))))))  

(defn list-metadata
  "database -> (seq-of metadata)
   Returns a sequence of all the metadata records in the database"
  [db]
  (jdbc/with-connection 
    db
    (let [ids (jdbc/with-query-results 
               res1 [(str "SELECT id FROM metadata")]
               (doall (map :id res1)))]
            (doall (map (fn [id] (read-metadata db id)) ids)))))

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
                               (do (update-translation db description-id1 (:description md))
                                   description-id1)
                               (if (:description md) (create-translation db (:description md))))]
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
      (if str-id (jdbc/delete-rows :translation ["id=?" str-id]))))
      true)
  
;;; Statements

(defn standard->integer
  [ps]
  {:pre [(contains? #{:pe, :cce, :brd, :dv} ps)]}
  (condp = ps
    :pe 0,
    :cce 1,
    :brd 2,
    :dv 3))

(defn integer->standard
  [i]
  {:pre [(integer? i)]}
  (condp = i
    0 :pe,
    1 :cce,
    2 :brd,
    3 :dv))
              
  
(defn create-statement 
  "Creates and inserts a statement record in the database for the atom of the given
   literal. Returns the id of the new statement record."
  [db literal]   
  {:pre [(literal? literal)]}
  (jdbc/with-connection 
    db
    (cond (sliteral? literal) 
          (first (vals (jdbc/insert-record
                         :statement {:atom (str (literal-atom literal))})))
          (statement? literal)
                (let [text-id (if (:text literal) (create-translation db (:text literal))),
                      header-id (if (:header literal) (create-metadata db (:header literal)))]
                  (first (vals (jdbc/insert-record
                                 :statement {:atom (str (:atom literal)),
                                             :header header-id,
                                             :weight (:weight literal),
                                             :main (:main literal),
                                             :standard (standard->integer (:standard literal)),
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
                      res [(str "SELECT * FROM translation WHERE id='" (:text s) "'")]
                   (if (empty? res) nil (first res))))
            pro (jdbc/with-query-results
                      res [(str "SELECT id FROM argument WHERE pro='true' AND conclusion='" id "'")]
                   (map :id (doall res)))
            con (jdbc/with-query-results
                      res [(str "SELECT id FROM argument WHERE pro='false' AND conclusion='" id "'")]
                   (map :id (doall res)))
            premise-of (jdbc/with-query-results
                      res [(str "SELECT argument FROM premise WHERE statement='" id "'")]
                   (map :argument (doall res))) ]
        (if s 
          (-> (make-statement)
              (merge s)
              (merge {:atom (read-string (:atom s))})
              (merge {:standard (integer->standard (:standard s))})
              (merge {:header h, 
                      :text t,
                      :pro pro,
                      :con con
                      :premise-of premise-of}))))))

(defn list-statements
  "database -> (seq-of statement)
   Returns a sequence of all the statement records in the database"
  [db]
  (jdbc/with-connection 
    db
    (let [ids (jdbc/with-query-results 
               res1 [(str "SELECT id FROM statement")]
               (doall (map :id res1)))]
            (doall (map (fn [id] (read-statement db id)) ids)))))

(defn statements-for-atom
  "database atom -> sequence of integer
   Queries the database to find statements with this
   atom. Returns a sequence of the ids of the statements
   found."
  [db atom]
  (jdbc/with-connection
    db
    (jdbc/with-query-results 
      res [(str "SELECT id FROM statement WHERE atom='" atom "'")]
      (doall (map :id res)))))

(defn get-statement
  "database literal -> integer
   If a statement for the atom of the literal exists in the database,
   the id of the first matching statement is returned, otherwise a new
   statement for the literal is first created and its id is
   returned."
  [db literal]
  {:pre [(literal? literal)]}
  (jdbc/with-connection db
   (or (first (statements-for-atom db (literal-atom literal)))
       (create-statement db literal))))
 
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
                               (do (update-translation db text-id1 (:text m))
                                   header-id1)
                               (if (:text m) (create-translation db (:text m))))]
        (condp = (first (jdbc/update-values
                          :statement
                          ["id=?" id]
                          (merge m {:header header-id2
                                    :text text-id2})))
          0 false,
          1 true))))
  

(defn delete-statement 
  "Deletes a statement entry with the given the id. Returns true."
  [db id]
  {:pre [(integer? id)]}
  (jdbc/with-connection 
    db 
    (let [text-id (jdbc/with-query-results 
                   res [(str "SELECT text FROM statement WHERE id='" id "'")]
                   (if (empty? res) nil (:text (first res))))
          header-id (jdbc/with-query-results 
                   res [(str "SELECT header FROM statement WHERE id='" id "'")]
                   (if (empty? res) nil (:header (first res))))]                   
      (jdbc/delete-rows :statement ["id=?" id])
      (if text-id (delete-translation db text-id))
      (if header-id (delete-metadata db header-id)))
      true))

  
;;; Premises

(defn create-premise 
  "Inserts a premise into a database.  
   Returns the id of the premise record in the database.
   Creates a statement for the literal of the premise if one
   does not already exist in the database."
  [db premise]
  {:pre [(premise? premise)]}
  (jdbc/with-connection 
    db
    (let [stmt-id (get-statement db (:statement premise))]    
      (first (vals (jdbc/insert-record 
                     :premise
                     (assoc premise :statement stmt-id)))))))

(defn read-premise 
  "database integer -> premise or nil
   Retrieves the premise with the given id from the database.
   Returns nil if no premise with the given id exists in the
   database."
  [db id]
  (jdbc/with-connection 
    db
    (jdbc/with-query-results 
      res1 [(str "SELECT * FROM premise WHERE id='" id "'")]
      (if (empty? (doall res1)) 
        nil 
        (let [m (first res1)
              stmt (read-statement db (:statement m))]
          (apply make-premise (flatten (seq (assoc m :statement stmt)))))))))

(defn list-premises
  "database -> (seq-of premise)
   Returns a sequence of all the premise records in the database"
  [db]
  (jdbc/with-connection 
    db
    (let [ids (jdbc/with-query-results 
               res1 [(str "SELECT id FROM premise")]
               (doall (map :id res1)))]
            (doall (map (fn [id] (read-premise db id)) ids)))))

(defn update-premise
  "database integer map -> boolean
   Updates the premise with the given id in the database
   with the values in the map. Returns true if the update
   succeeds."
  [db id m]
  {:pre [(integer? id) (map? m)]}
  (jdbc/with-connection
    db
    (let [stmt (if (:literal m)
                 (get-statement db (:literal m)))
          m2 (if (nil? stmt)
               m
               (-> m
                   (dissoc :literal)
                   (assoc :statement stmt)))]                               
      (condp = (first (jdbc/update-values :premise ["id=?" id] m2))
        0 false
        1 true))))            

(defn delete-premise 
  "Deletes a premise with the given the id"
  [db id]
  (jdbc/with-connection db
    (jdbc/delete-rows :premise ["id=?" id]))
  true)
                     
;;; Arguments
                           
(defn create-argument 
  "Creates a one-step argument and inserts it into a database.  Returns
   the id of the new argument."
  [db arg]
  {:pre [(argument? arg)]}
  (jdbc/with-connection 
    db
    (let [conclusion-id (get-statement db (:conclusion arg))
          header-id (if (:header arg) (create-metadata db (:header arg)))
          arg-id (first (vals (jdbc/insert-record 
                     :argument
                     {:conclusion conclusion-id,
                      :pro (literal-pos? (:conclusion arg))
                      :strict (:strict arg),
                      :weight (:weight arg), 
                      :scheme (:scheme arg),
                      :header header-id})))]
      (doseq [p (:premises arg)]
        (update-premise
          db 
          (create-premise db p)
          {:argument arg-id}))
      arg-id)))

(defn read-argument
  "database integer -> argument or nil
   Retrieves the argument with the given id from the database.
   Returns nil if no argument with the given id exists in the
   database."
  [db id]
  (jdbc/with-connection 
    db
    (jdbc/with-query-results 
      res1 [(str "SELECT * FROM argument WHERE id='" id "'")]
      (if (empty? res1) 
        nil 
        (let [m (first res1)
              conclusion (read-statement db (:conclusion m))
              header (if (:header m) (read-metadata db (:header m)))
              premises (jdbc/with-query-results 
                         res1 [(str "SELECT id FROM premise WHERE argument='" id "'")]
                         (doall (map (fn [id] (read-premise db id))
                                     (map :id res1))))]
          (-> (apply make-argument (flatten (seq (assoc m :conclusion conclusion))))
              (assoc :header header
                     :premises premises)))))))
          
(defn list-arguments
  "database -> (seq-of argument)
   Returns a sequence of all the argument records in the database"
  [db]
  (jdbc/with-connection 
    db
    (let [ids (jdbc/with-query-results 
               res1 [(str "SELECT id FROM argument")]
               (doall (map :id res1)))]
            (doall (map (fn [id] (read-argument db id)) ids)))))

(defn update-argument
  "database integer map -> boolean
   Updates the argument with the given id in the database
   with the values in the map. Returns true if the update
   succeeds."
  [db id m]
  {:pre [(integer? id) (map? m)]}
  (jdbc/with-connection
    db
    (let [header-id (if (:header m)
                      (or (jdbc/with-query-results 
                            res1 [(str "SELECT header FROM argument WHERE id='" id "'")]
                            (:header (first res1)))
                          (create-metadata db (make-metadata)))),         
          conclusion-id (if (:conclusion m)
                          (get-statement db (:conclusion m)))] 
      (if (:premises m)    
        (do 
          ; first delete existing premises
          (jdbc/with-query-results 
            res1 [(str "SELECT id FROM premise WHERE argument='" id "'")]
            (doseq [p res1] (delete-premise db (:id p))))   
          ; then create and link the new premises 
          (doseq [p (:premises m)]
            (update-premise
              db 
              (create-premise db p)
              {:argument id}))))                           
      (condp = (first (jdbc/update-values 
                        :argument 
                        ["id=?" id] 
                        (merge m (if (:conclusion m)
                                   {:header header-id
                                    :conclusion conclusion-id}
                                   {:header header-id}))))
        0 false
        1 true))))          

(defn delete-argument 
  "Deletes an argument with the given the id.  The statements
   of the conclusion and premises of the argument are not 
   deleted. Returns true if successful."
  [db id]
  (jdbc/with-connection 
    db
    ; first delete the premises of argument
    (jdbc/with-query-results 
      res1 [(str "SELECT id FROM premise WHERE argument='" id "'")]
      (doseq [p res1] (delete-premise db (:id p))))
    ; now delete the header of the argument, if it has one
    (jdbc/with-query-results 
      res1 [(str "SELECT header FROM argument WHERE id='" id "'")]
      (if (:header (first res1))
        (delete-metadata db (:header (first res1)))))
    ; finally delete the argument itself
    (jdbc/delete-rows :argument ["id=?" id]))
  true)

;;; Namespaces
  
(defn create-namespace
  "database map -> boolean
   Creates a namespace record and inserts it into a database.  
   Returns the prefix of the namespace, which serves as its key."
  [db m]   
  {:pre [(map? m)]}
  (jdbc/with-connection db
     (let [result (jdbc/insert-record :namespace m)]
       (first (vals result))))
  (:prefix m))

(defn read-namespace 
  "database string -> map or nil
   Retrieves the namespace with the given prefix from the database.
   Returns nil if no namespace with the given prefix exists in the
   database."
  [db prefix]
  (jdbc/with-connection 
    db
    (jdbc/with-query-results 
      res1 [(str "SELECT * FROM namespace WHERE prefix='" prefix "'")]
      (if (empty? res1) nil (first res1)))))

(defn list-namespaces
  "database -> (seq-of map)
   Returns a sequence of maps representing the namespaces in the database"
  [db]
  (jdbc/with-connection 
    db
    (jdbc/with-query-results 
      res1 [(str "SELECT * FROM namespace")]
      (doall res1))))
  
(defn update-namespace 
  "database integer map -> boolean
   Updates the namespace with the given id in the database
   with the values in the map. Returns true if the update
   succeeds."
  [db id m]
  {:pre [(integer? id) (map? m)]}
  (jdbc/with-connection
    db
    (condp = (first (jdbc/update-values :namespace ["id=?" id] m))
      0 false
      1 true)))

(defn delete-namespace 
  "Deletes a translation with the given the prefix.
   Returns true if successful."
  [db prefix]
  (jdbc/with-connection db
    (jdbc/delete-rows :namespace ["prefix=?" prefix]))
  true)

;;; Statement Polls

(defn create-statement-poll
  "Records votes for the given user, 
   where the votes are a map from statement ids to real
   numbers in the range 0.0 to 1.0. All other votes for 
   this user, if any, are deleted."
  [db userid votes]
  {:pre [(string? userid) 
         (map? votes)
         (every? (fn [x] 
                   (and (number? x)
                        (<= 0.0 x 1.0)))
                 (vals votes))]}
  (jdbc/with-connection 
    db
    (doseq [vote votes]
      (clojure.java.jdbc/insert-values
        :stmtpoll
        [:userid :statement :opinion]
        [userid (first vote) (second vote)]))
    true))

(defn read-statement-poll
  "database int -> {:count x, :value y}
   Returns the results of a poll for the statement
   with the given id.  The results are
   returned as a map with a count of the number of
   votes and the average value of the votes."
  [db statement-id]
  {:pre [(integer? statement-id)]}
  (jdbc/with-connection 
      db
     {:count (jdbc/with-query-results 
      res1 [(str "SELECT COUNT(statement) FROM stmtpoll WHERE statement='" statement-id "'")]
       (second (first (first res1)))),
      :value (jdbc/with-query-results 
      res1 [(str "SELECT AVG(opinion) FROM stmtpoll WHERE statement='" statement-id "'")]
       (second (first (first res1))))}))

(defn list-statement-poll
  "database -> (seq-of map)
   Returns a sequence of maps representing the statement poll table in the database"
  [db]
  (jdbc/with-connection 
    db
    (jdbc/with-query-results 
      res1 [(str "SELECT * FROM stmtpoll")]
      (doall res1))))
       
(defn update-statement-poll
  "database string map -> boolean
   Updates the votes of a user.  The map is
   from statement ids to numbers in the range of 0.0 to 1.0."
  [db userid votes] 
  {:pre [(string? userid) 
         (map? votes)
         (every? (fn [x] 
                   (and (number? x)
                        (<= 0.0 x 1.0)))
                 (vals votes))]}
  (jdbc/with-connection 
    db
    (doseq [vote votes]
      (jdbc/update-or-insert-values 
        :stmtpoll 
        ["userid=? AND statement=?" userid (first vote)] 
        {:userid userid,
         :statement (first vote) 
         :opinion (second vote)})))
  true)

(defn delete-statement-poll 
  "Deletes all votes for the given statement id."
  [db statement-id]
  {:pre [(integer? statement-id)]}
  (jdbc/with-connection db
    (jdbc/delete-rows :stmtpoll ["statement=?" statement-id]))
  true)

;;; Argument Polls

(defn create-argument-poll
  "Records votes for the given user, 
   where the votes are a map from argument ids to real
   numbers in the range 0.0 to 1.0. All other votes for 
   this user, if any, are deleted."
  [db userid votes]
  {:pre [(string? userid) 
         (map? votes)
         (every? (fn [x] 
                   (and (number? x)
                        (<= 0.0 x 1.0)))
                 (vals votes))]}
  (jdbc/with-connection 
    db
    (doseq [vote votes]
      (clojure.java.jdbc/insert-values
        :argpoll
        [:userid :argument :opinion]
        [userid (first vote) (second vote)]))
    true))

(defn read-argument-poll
  "database int -> {:count x, :value y}
   Returns the results of a poll for the argument
   with the given id.  The results are
   returned as a map with a count of the number of
   votes and the average value of the votes."
  [db arg-id]
  {:pre [(integer? arg-id)]}
  (jdbc/with-connection 
      db
     {:count (jdbc/with-query-results 
      res1 [(str "SELECT COUNT(argument) FROM argpoll WHERE argument='" arg-id "'")]
       (second (first (first res1)))),
      :value (jdbc/with-query-results 
      res1 [(str "SELECT AVG(opinion) FROM argpoll WHERE argument='" arg-id "'")]
       (second (first (first res1))))}))

(defn list-argument-poll
  "database -> (seq-of map)
   Returns a sequence of maps representing the argument poll table in the database"
  [db]
  (jdbc/with-connection 
    db
    (jdbc/with-query-results 
      res1 [(str "SELECT * FROM argpoll")]
      (doall res1))))
       
(defn update-argument-poll
  "database string map -> boolean
   Updates the votes of a user.  The map is
   from argument ids to numbers in the range of 0.0 to 1.0."
  [db userid votes] 
  {:pre [(string? userid) 
         (map? votes)
         (every? (fn [x] 
                   (and (number? x)
                        (<= 0.0 x 1.0)))
                 (vals votes))]}
  (jdbc/with-connection 
    db
    (doseq [vote votes]
      (jdbc/update-or-insert-values 
        :argpoll 
        ["userid=? AND argument=?" userid (first vote)] 
        {:userid userid,
         :argument (first vote) 
         :opinion (second vote)})))
  true)

(defn delete-argument-poll 
  "Deletes all votes for the given argument id."
  [db arg-id]
  {:pre [(integer? arg-id)]}
  (jdbc/with-connection db
    (jdbc/delete-rows :argpoll ["argument=?" arg-id]))
  true)

