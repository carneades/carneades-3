;; Copyright (c) 2011 Fraunhofer Gesellschaft
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns ^{:doc "A database schema and CRUD functions for storing arguments
             persistently in a relational database.

             It includes the polls about whether users agree or disagree with
             statements and the weights of arguments.

             Naming convention: get-x operations return ids; read-x
             operations return a structure with properties of an
             object."}
  ;; most of the bugs come from this namespace.
  ;; it would be good to rewrite it. Maybe with korma.
  carneades.database.argument-graph
  (:use clojure.pprint
        [carneades.database.db :only [with-db make-connection]]
        carneades.engine.dublin-core
        carneades.engine.statement
        carneades.engine.argument
        carneades.engine.utils)
  (:require [clojure.java.jdbc.deprecated :as jdbc]
            [clojure.string :as s]
            [clojure.walk :as w]
            [carneades.engine.uuid :as uuid]
            [taoensso.timbre :as timbre :refer [debug info spy]]))

(declare create-metadata)

(defn create-argument-database
  "Initialize the database by creating the tables. Metadata can be
   provided to describe the debate being modeled in the database and
   the creators of the model, among other information.
   Returns true if the database is successul created and initialized"
  [project-name db-name root-username root-password metadata]
  (let [db  (make-connection project-name
                             db-name
                             root-username
                             root-password
                             :create true)]
    (jdbc/with-connection
      db
      (jdbc/transaction

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
          [:id "int identity"]    ; local id, see also key and identifier
          [:key "varchar unique"] ; citation key, optional
          [:contributor "varchar"]
          [:coverage "varchar"]
          [:creator "varchar"]
          [:date "varchar"]       ; http://www.w3.org/TR/NOTE-datetime
          [:description "int"]
          [:format "varchar"]     ; A list of MIME types, semicolon separated
          [:identifier "varchar"] ; URI, DOI or something similar
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
          [:id "varchar primary key not null"] ; a URN in the UUID namespace
          [:weight "double default null"]
          [:value "double default null"]
          [:standard "tinyint default 0"]   ; 0=pe, 1=cce, 2=brd, 3=dv
          [:atom "varchar"]                 ; Clojure s-expression
          [:text "int"]
          [:main "boolean default false"]   ; true if a main issue
          [:header "int"]
          ["foreign key(text) references translation(id)"]
          ["foreign key(header) references metadata(id)"])

        (jdbc/create-table
         :argument
         [:id "varchar primary key not null"] ; a URN in the UUID namespace
          [:conclusion "varchar not null"]     ; URN of the conclusion
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
          [:argument "varchar"]              ; null allowed, so as to be able to create premises first
          [:statement "varchar not null"]    ; URN
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
          [:statement "varchar not null"]
          [:opinion "double default 0.5"]
          ["foreign key(statement) references statement(id)"])

        (jdbc/create-table
          :argpoll         ; argument poll
          [:userid "varchar not null"]
          [:argument "varchar not null"]
          [:opinion "double default 0.5"]
          ["foreign key(argument) references argument(id)"])

        ;; Grant read access to the public role
        ;; and create a guest user account with this role.
        (jdbc/do-commands "grant select on translation, metadata, statement, argument,
                           premise, namespace to public"
                          "create user guest password ''"
                          "grant public to guest")

        (create-metadata metadata)

        true))))


;;; Translations

(defn create-translation
  "map -> integer
   Creates a translation record and inserts it into a database.
   Returns the id of the new translation."
  [m]
  {:pre [(map? m)]}
  (first (vals (jdbc/insert-record :translation m))))

(defn read-translation
  "integer -> map or nil
   Retrieves the translation with the given id from the database.
   Returns nil if no translation with the given id exists in the
   database."
  [id]
  (jdbc/with-query-results
    res1 ["SELECT * FROM translation WHERE id=?" id]
    (if (empty? res1) nil (first res1))))

(defn list-translations
  "Returns a sequence of all the translation records in the database"
  []
  (jdbc/with-query-results
      res1 ["SELECT * FROM translation"]
      (doall res1)))

(defn update-translation
  "integer map -> boolean
   Updates the translation with the given id in the database
   with the values in the map. Returns true if the update
   succeeds."
  [id m]
  {:pre [(integer? id) (map? m)]}
  (condp = (first (jdbc/update-values :translation ["id=?" id] m))
    0 false
    1 true))

(defn delete-translation
  "Deletes a translation with the given the id"
  [id]
  (jdbc/delete-rows :translation ["id=?" id]))

;;; Metadata

;; Note: the first metadata record describes
;; the database as whole. This record is created
;; when the database is created and initialized.

(defn create-metadata
  "Inserts a metadata structure into a database.
   Returns the id of the record in the database."
  [metadata]
  (let [str-id (if (:description metadata)
                 (create-translation (:description metadata)))]
    (first (vals (jdbc/insert-record
                   :metadata
                   (if str-id
                     (assoc metadata :description str-id)
                     metadata))))))

(defn read-metadata
  "Retrieves the metadata with the given id from the database,
   and returns it as a metadata record. Returns nil if there
   is no metadata record in the database with this id."
  [id]
  (let [md (jdbc/with-query-results
             res1 ["SELECT * FROM metadata WHERE id=?" id]
             (if (empty? res1)
               nil
               (dissoc (first res1) :id)))
        d (when (:description md)
            (jdbc/with-query-results
              res2 ["SELECT * FROM translation WHERE id=?" (:description md)]
              (if (empty? res2)
                nil
                (dissoc (first res2) :id))))]
    (cond (nil? md) nil
          d (assoc (map->metadata md) :description d)
          :else (dissoc (map->metadata md) :description))))

(defn list-metadata
  "Returns a sequence of all the metadata records in the database"
  []
  (let [ids (jdbc/with-query-results
              res1 ["SELECT id FROM metadata"]
              (doall (map :id res1)))]
    (doall (map (fn [id] (read-metadata id)) ids))))

(defn update-metadata
  "Updates the metadata record with the given id in the database with the values
   in the map.  Returns true if the update was successful."
  [id md]
  {:pre [(map? md)]}
  (let [existing-description-id (jdbc/with-query-results
                                  res ["SELECT description FROM metadata WHERE id=?" id]
                                  (if (empty? res) nil (:description (first res))))
        new-description-id (cond (and (:description md) existing-description-id)
                                 (do (update-translation existing-description-id
                                                         (:description md))
                                     existing-description-id)
                                 (:description md) (create-translation (:description md))
                                 :else existing-description-id)
        delta (merge md {:description new-description-id})]
    (condp = (first (jdbc/update-values
                      :metadata
                      ["id=?" id]
                      delta))
      0 false
      1 true)))

(defn delete-metadata
  "Deletes a metadata entry with the given the id. Returns true."
  [id]
  {:pre [(integer? id)]}
  (let [str-id (jdbc/with-query-results
                 res ["SELECT description FROM metadata WHERE id=?" id]
                 (if (empty? res) nil (:description (first res))))]
    (jdbc/delete-rows :metadata ["id=?" id])
    (if str-id (jdbc/delete-rows :translation ["id=?" str-id]))
    true))

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
  [literal]
  (cond (sliteral? literal)
        (let [id (if (uuid/urn-symbol? (literal-atom literal))
                   (str (literal-atom literal))
                   (uuid/make-urn))]
          (jdbc/insert-record
           :statement {:id id
                       :atom (serialize-atom (literal-atom literal))})
          id)
        (statement? literal)
        (let [id (if (uuid/urn-symbol? (:id literal))
                     (str (:id literal))
                     (if (uuid/urn-symbol? (literal-atom literal))
                         (str (literal-atom literal))
                         (uuid/make-urn)))
              text-id (if (:text literal) (create-translation (:text literal))),
              header-id (if (:header literal) (create-metadata (:header literal)))]
          (jdbc/insert-record
           :statement {:id id
                        :atom (serialize-atom (:atom literal))
                        :header header-id
                        :weight (:weight literal)
                        :value (:value literal)
                        :main (:main literal)
                        :standard (standard->integer (:standard literal))
                        :text text-id})
          id)))

(defn read-statement
  "string -> statement or nil
   Retrieves the statement with the give id from the database.
   Returns nil if there is not statement with this id."
  [id]
  {:pre [(string? id)]}
  (let [s (jdbc/with-query-results
            res ["SELECT * FROM statement WHERE id=?" id]
            (if (empty? res) nil (first res)))
        h (if (:header s) (read-metadata (:header s)))
        t (if (:text s) (jdbc/with-query-results
                          res ["SELECT * FROM translation WHERE id=?" (:text s)]
                          (if (empty? res) nil (first res))))
        pro (jdbc/with-query-results
              res ["SELECT id FROM argument WHERE pro='true' AND conclusion=?" id]
              (map :id (doall res)))
        con (jdbc/with-query-results
              res ["SELECT id FROM argument WHERE pro='false' AND conclusion=?" id]
              (map :id (doall res)))
        premise-of (jdbc/with-query-results
                     res ["SELECT argument FROM premise WHERE statement=?" id]
                     (map :argument (doall res))) ]
    (when s
      (-> (make-statement :id (symbol id))
          (merge (dissoc s :id))
          (merge {:atom (when (and (:atom s) (not (empty? (:atom s))))
                          (unserialize-atom (:atom s)))})
          (merge {:standard (integer->standard (:standard s))})
          (merge {:header h,
                  :text t,
                  :pro (map symbol pro),
                  :con (map symbol con)
                  :premise-of (map symbol premise-of)})))))

(defn list-statements
  "Returns a sequence of all the statement records in the database"
  []
  (let [ids (jdbc/with-query-results
              res1 ["SELECT id FROM statement"]
              (doall (map :id res1)))]
    (doall (map (fn [id] (read-statement id)) ids))))

(defn main-issues
  "Returns a sequence of statements that are main issues."
  []
  (let [ids (jdbc/with-query-results
              res1 ["SELECT id FROM statement WHERE main='true'"]
              (doall (map :id res1)))]
    (doall (map (fn [id] (read-statement id)) ids))))

(defn statements-for-atom
  "atom -> sequence of strings
   Queries the database to find statements with this
   atom. Returns a sequence of the ids of the statements
   found."
  [atom]
  (jdbc/with-query-results
    res [(str "SELECT id FROM statement WHERE atom='" (str atom) "'")]
    (doall (map :id res))))

(defn premises-for-statement
  "Queries the database to find premises with this statement's id.
   Returns a sequence of ids."
  [id]
  (jdbc/with-query-results
    res ["SELECT id FROM premise WHERE statement=?" id]
    (doall (map :id res))))

(defn statement-created?
  "Returns true if the statement has been created in the database"
  [statement]
  (jdbc/with-query-results
    res ["SELECT id FROM statement WHERE id=?" (str (:id statement))]
    (seq res)))

(defn get-statement
  "literal -> string
   If a statement for the atom of the literal exists in the database,
   or a statement for the id of the statement,
   the id of the first matching statement is returned, otherwise a new
   statement for the literal is first created and its id is
   returned."
  [literal]
  (cond (uuid/urn-symbol? literal)
        (str literal)

        (and (statement-created? literal) (:id literal))
        (:id literal)
        
        (and (statement-created? literal) (:atom literal))
        (first (statements-for-atom (literal-atom literal)))
        
        :else (create-statement literal)))

(defn update-statement
  "string map -> boolean
   Updates the statement record with the given id in the
   database with the values in the map. Returns true if
   the update was successful."
  [id m]
  {:pre [(map? m)]}
  (let [m (dissoc m :positive :premise-of :pro :con)
        existing-header-id (if (:header m)
                             (jdbc/with-query-results
                               res ["SELECT header FROM statement WHERE id=?" id]
                               (if (empty? res) nil (:header (first res)))))
        updating-header (and (:header m) existing-header-id)
        new-header (if existing-header-id
                     (do (update-metadata existing-header-id (:header m))
                         existing-header-id)
                     (when (:header m)
                       (create-metadata (merge (make-metadata) (:header m)))))
        text-id1 (when (:text m)
                   (jdbc/with-query-results
                     res ["SELECT text FROM statement WHERE id=?" id]
                     (if (empty? res) nil (:text (first res)))))
        updating-text (and (:text m) text-id1)
        text-id2  (if text-id1
                    (do (update-translation text-id1 (:text m))
                        text-id1)
                    (when (:text m)
                      (create-translation (:text m))))
        standard (if (:standard m)
                   (standard->integer (keyword (:standard m)))
                   0)
        delta (cond (or (and updating-header updating-text)
                        (and (:header m) text-id2))
                    {:header new-header
                     :text text-id2
                     :standard standard}
                    
                    (:header m)
                    {:header new-header
                     :standard standard}
                    
                    text-id2 {:text text-id2
                              :standard standard})]
    (condp = (first (jdbc/update-values
                      :statement
                      ["id=?" id]
                      (merge m delta)))
      0 false,
      1 true)))


(defn delete-statement
  "Deletes a statement entry with the given the id. Returns true."
  [id]
  {:pre [(string? id)]}
  (let [text-id (jdbc/with-query-results
                  res ["SELECT text FROM statement WHERE id=?" id]
                  (if (empty? res) nil (:text (first res))))
        header-id (jdbc/with-query-results
                    res ["SELECT header FROM statement WHERE id=?" id]
                    (if (empty? res) nil (:header (first res))))]
    (jdbc/delete-rows :statement ["id=?" id])
    (if text-id (delete-translation text-id))
    (if header-id (delete-metadata header-id))
    true))

;;; Premises

(defn create-premise
  "Inserts a premise into a database.
   Returns the id of the premise record in the database.
   Creates a statement for the literal of the premise if one
   does not already exist in the database."
  [premise & [argid]]
  (let [stmt-id (get-statement (:statement premise))
        ;; we dissoc :pro or :con which are not in the Premise record
        ;; but are added from read-premise!
        premise (dissoc premise :pro :con)
        premise (assoc premise :statement stmt-id)
        premise (if argid
                  (assoc premise :argument argid)
                  premise)]
    (first (vals (jdbc/insert-record
                  :premise premise)))))

(declare get-pro-arguments get-con-arguments)

(defn read-premise
  "database integer -> premise or nil
   Retrieves the premise with the given id from the database.
   Returns nil if no premise with the given id exists in the
   database. The resulting premises has additional properties
   listing the ids of the arguments pro and con the statement of
   the premise."
  [id]
  (jdbc/with-query-results
    res1 ["SELECT * FROM premise WHERE id=?" id]
    (if (empty? (doall res1))
      nil
      (let [m (dissoc (first res1) :id)
            stmt (read-statement (:statement m))
            pro (get-pro-arguments (:statement m))
            con (get-con-arguments (:statement m))]
        (merge (map->premise {:statement stmt
                        :positive (:positive m)
                        :role (:role m)
                              :implicit (:implicit m)})
               {:pro pro :con con})))))

(defn list-premises
  "Returns a sequence of all the premise records in the database"
  []
  (let [ids (jdbc/with-query-results
              res1 ["SELECT id FROM premise"]
              (doall (map :id res1)))]
    (doall (map (fn [id] (read-premise id)) ids))))

(defn update-premise
  "integer map -> boolean
   Updates the premise with the given id in the database
   with the values in the map. Returns true if the update
   succeeds."
  [id m]
  {:pre [(integer? id) (map? m)]}
  (let [stmt (if (:literal m)
               (get-statement (:literal m)))
        m2 (if (nil? stmt)
             m
             (-> m
                 (dissoc :literal)
                 (assoc :statement stmt)))]
    (condp = (first (jdbc/update-values :premise ["id=?" id] m2))
      0 false
      1 true)))

(defn delete-premise
  "Deletes a premise with the given the id"
  [id]
  (jdbc/delete-rows :premise ["id=?" id])
  true)

;;; Arguments

(defn create-argument
  "Creates a one-step argument and inserts it into a database.  Returns
   the id of the new argument."
  [arg]
  (let [arg-id (str (:id arg)),
        scheme-id (str (serialize-atom (:scheme arg)))
        conclusion-id (get-statement (:conclusion arg)),
        header-id (if (:header arg) (create-metadata (:header arg)))]
    (jdbc/insert-record
      :argument
      (assoc (dissoc arg :premises :exceptions)
             :id arg-id
             :scheme scheme-id
             :conclusion conclusion-id
             :header header-id))
    (doseq [p (:premises arg)]
      (create-premise (assoc p :argument arg-id)))
    arg-id))

(defn get-rebuttals
  "string -> sequence of string
   Returns a sequence of ids of arguments which rebut
   the argument with the given id."
  [arg-id]
  (jdbc/with-query-results
    res1 ["SELECT * FROM argument WHERE id=?" arg-id]
    (if (empty? res1)
      []
      (let [m (first res1)]
        (jdbc/with-query-results
          res2 ["SELECT id FROM argument WHERE conclusion=? AND pro=?"
                (:conclusion m)
                (not (:pro m))]
          (doall (map :id res2)))))))

(defn get-undercutters
  "string -> sequence of string
   Returns a sequence of ids of arguments which undercut
   the argument with the given id."
  [arg-id]
  (jdbc/with-query-results
    res1 ["SELECT id FROM statement WHERE atom=?" (format "(valid %s)" arg-id)]
    (if (empty? res1)
      []
      (let [stmt (first res1)
            args
            (jdbc/with-query-results
              res2 ["SELECT id FROM argument WHERE conclusion=?" (:id stmt)]
              (doall (map :id res2)))]
        (filter #(not (:pro %)) args)))))

(defn get-dependents
  "string -> sequence of string
   returns a sequence of argument ids in which the conclusion of the argument
   with the given id is a premise"
  [arg-id]
  (jdbc/with-query-results
    res1 ["SELECT conclusion FROM argument WHERE id=?" arg-id]
    (if (empty? res1)
      []
      (let [stmt (first res1)]
        (jdbc/with-query-results
          res2 ["SELECT argument FROM premise WHERE statement=?" (:id stmt)]
          (doall (map :id res2)))))))

(defn read-argument
  "string -> argument or nil
   Retrieves the argument with the given id from the database.
   Returns nil if no argument with the given id exists in the
   database. The resulting argument has additional properties
   listing the ids of the rebuttals and undercutters of the argument."
  [id]
  {:pre [(string? id)]
   :post [(not (string? (:scheme %)))]}
  (jdbc/with-query-results
    res1 ["SELECT * FROM argument WHERE id=?" id]
    (if (empty? (doall res1))
      nil
      (let [m (first res1)
            conclusion (read-statement (:conclusion m))
            header (if (:header m) (read-metadata (:header m)))
            premises (jdbc/with-query-results
                       res1 ["SELECT id FROM premise WHERE argument=?" id]
                       (doall (map (fn [id]
                                     (read-premise id))
                                   (map :id res1))))
            rs (get-rebuttals id)
            us (get-undercutters id)
            ds (get-dependents id)
            scheme (unserialize-atom (:scheme m))]
        (map->argument (assoc m
                         :id (symbol id)
                         :scheme scheme
                         :conclusion conclusion
                         :header header
                         :premises premises
                         :rebuttals (map symbol rs)
                         :undercutters (map symbol us)
                         :dependents (map symbol ds)))))))

(defn list-arguments
  "Returns a sequence of all the argument records in the database"
  []
  (let [ids (jdbc/with-query-results
              res1 ["SELECT id FROM argument"]
              (doall (map :id res1)))]
    (doall (map (fn [id] (read-argument id)) ids))))

(defn get-pro-arguments
  "Returns a sequence of the ids of arguments pro
   the statement with the given id."
  [stmt-id]
  (jdbc/with-query-results
              res1 [(str "SELECT id FROM argument WHERE pro='true' AND conclusion='" stmt-id "'")]
              (doall (map :id res1))))

(defn get-con-arguments
  "Returns a sequence of the ids of arguments con
   the statement with the given id."
  [stmt-id]
  (jdbc/with-query-results
              res1 ["SELECT id FROM argument WHERE pro='false' AND conclusion=?" stmt-id]
              (doall (map :id res1))))

(defn update-argument
  "string map -> boolean
   Updates the argument with the given id in the database
   with the values in the map. Returns true if the update
   succeeds."
  [id m]
  {:pre [(string? id) (map? m)]}
  (let [m (dissoc m :dependents :rebuttals :exceptions :undercutters)
        header-id1 (if (:header m)
                    (or (jdbc/with-query-results
                          res1 ["SELECT header FROM argument WHERE id=?" id]
                          (:header (first res1)))
                        (create-metadata (make-metadata))))
        header-id2 (if header-id1
                     (do (update-metadata header-id1 (:header m))
                         header-id1)
                     (when (:header m)
                       (create-metadata (merge (make-metadata) (:header m)))))
        conclusion-id (when (:conclusion m)
                        (get-statement (:conclusion m)))]
    (when (:premises m)
      ;; first delete existing premises
      (jdbc/with-query-results
        res1 ["SELECT id FROM premise WHERE argument=?" id]
        (doseq [p res1]
          (delete-premise (:id p))))
      ;; then create and link the new premises
      (doseq [p (:premises m)]
        (create-premise p id)))
    (let [m (dissoc m :premises)
          m (merge m (cond (and conclusion-id header-id2)
                           {:header header-id2
                            :conclusion conclusion-id}

                           header-id2
                           {:header header-id2}))
          m (if (:scheme m)
              (update-in m [:scheme] serialize-atom)
              m)]
      (if (empty? m)
        true
        (condp = (first (jdbc/update-values
                         :argument
                         ["id=?" id]
                         m))
          0 false
          1 true)))))

(defn delete-argument
  "Deletes an argument with the given the id.  The statements
   of the conclusion and premises of the argument are not
   deleted. Returns true if successful."
  [id]
  ; first delete the premises of argument
  (jdbc/with-query-results
    res1 ["SELECT id FROM premise WHERE argument=?" (str id)]
    (doseq [p res1] (delete-premise (:id p))))
  ; finally delete the argument itself
  (jdbc/delete-rows :argument ["id=?" (str id)])
  ; now delete the header of the argument, if it has one
  (jdbc/with-query-results
    res1 ["SELECT header FROM argument WHERE id=?" (str id)]
    (if (:header (first res1))
      (delete-metadata (:header (first res1)))))
  true)

;;; Namespaces

(defn create-namespace
  "map -> boolean
   Creates a namespace record and inserts it into a database.
   Returns the prefix of the namespace, which serves as its key."
  [m]
  {:pre [(map? m)]}
  (jdbc/insert-record :namespace m)
  (:prefix m))

(defn read-namespace
  "string -> map or nil
   Retrieves the namespace with the given prefix from the database.
   Returns nil if no namespace with the given prefix exists in the
   database."
  [prefix]
  (jdbc/with-query-results
    res1 ["SELECT * FROM namespace WHERE prefix=?" prefix]
    (if (empty? res1) nil (first res1))))

(defn list-namespaces
  "Returns a sequence of maps representing the namespaces in the database"
  []
  (jdbc/with-query-results
    res1 ["SELECT * FROM namespace"]
    (doall res1)))

(defn update-namespace
  "integer map -> boolean
   Updates the namespace with the given id in the database
   with the values in the map. Returns true if the update
   succeeds."
  [id m]
  {:pre [(integer? id) (map? m)]}
  (condp = (first (jdbc/update-values :namespace ["id=?" id] m))
    0 false
    1 true))

(defn delete-namespace
  "Deletes a translation with the given the prefix.
   Returns true if successful."
  [prefix]
  (jdbc/delete-rows :namespace ["prefix=?" prefix])
  true)

;;; Statement Polls

(defn create-statement-poll
  "Records votes for the given user,
   where the votes are a map from statement ids to real
   numbers in the range 0.0 to 1.0. All other votes for
   this user, if any, are deleted."
  [poll]
  {:pre [(string? (:id poll))
         (map? (:votes poll))
         (every? (fn [x]
                   (and (number? x)
                        (<= 0.0 x 1.0)))
                 (vals (:votes poll)))]}
  (let [{:keys [id votes]} poll]
    (doseq [[statement opinion] votes]
      (jdbc/insert-record
       :stmtpoll
       {:userid id
        :statement (name statement)
        :opinion opinion}))
    id))

(defn read-statement-poll
  "Retrieves the poll with the given id from the database"
  [id]
  (jdbc/with-query-results
    res1 ["SELECT * FROM stmtpoll WHERE userid=?" (str id)]
    {:votes (zipmap (map :statement res1) (map :opinion res1))
     :id id}))

(defn read-poll-for-statement
  "int -> {:count x, :value y}
   Returns the results of a poll for the statement
   with the given id.  The results are
   returned as a map with a count of the number of
   votes and the average value of the votes."
  [statement-id]
  {:pre [(integer? statement-id)]}
  {:count (jdbc/with-query-results
            res1 ["SELECT COUNT(statement) FROM stmtpoll WHERE statement=?" statement-id]
            (second (first (first res1)))),
   :value (jdbc/with-query-results
            res1 ["SELECT AVG(opinion) FROM stmtpoll WHERE statement=?" statement-id]
            (second (first (first res1))))})

(defn list-statement-poll
  "Returns a sequence of maps representing the statement poll table in the database"
  []
  (jdbc/with-query-results
    res1 ["SELECT * FROM stmtpoll"]
    (doall res1)))

(defn update-statement-poll
  "string map -> boolean
   Updates the votes of a user.  The map is
   from statement ids to numbers in the range of 0.0 to 1.0."
  [poll]
  {:pre [(string? (:id poll))
         (map? (:votes poll))
         (every? (fn [x]
                   (and (number? x)
                        (<= 0.0 x 1.0)))
                 (vals (:votes poll)))]}
  (let [{:keys [id votes]} poll]
   (doseq [[statement opinion] votes]
     (jdbc/update-or-insert-values
      :stmtpoll
      ["userid=? AND statement=?" (str id) (name statement)]
      {:userid id,
       :statement (name statement)
       :opinion opinion}))
   id))

(defn delete-statement-poll
  "Deletes all votes for the given statement id."
  [statement-id]
  {:pre [(integer? statement-id)]}
  (jdbc/delete-rows :stmtpoll ["statement=?" statement-id])
  true)

;;; Argument Polls

(defn create-argument-poll
  "Records votes for the given user,
   where the votes are a map from argument ids to real
   numbers in the range 0.0 to 1.0. All other votes for
   this user, if any, are deleted."
  [arg]
  {:pre [(string? (:id arg))
         (map? (:votes arg))
         (every? (fn [x]
                   (and (number? x)
                        (<= 0.0 x 1.0)))
                 (vals (:votes arg)))]}
  (let [{:keys [id votes]} arg]
   (doseq [[argument opinion] votes]
     (jdbc/insert-record
      :argpoll
      {:userid id
       :argument (name argument)
       :opinion opinion}))
   id))

(defn read-argument-poll
  "Retrieves the poll with the given id from the database"
  [id]
  (jdbc/with-query-results
    res1 ["SELECT * FROM argpoll WHERE userid=?" (str id)]
    {:votes (zipmap (map :argument res1) (map :opinion res1))
     :id id}))

(defn read-poll-for-argument
  "int -> {:count x, :value y}
   Returns the results of a poll for the argument
   with the given id.  The results are
   returned as a map with a count of the number of
   votes and the average value of the votes."
  [arg-id]
  {:pre [(integer? arg-id)]}
  {:count (jdbc/with-query-results
            res1 ["SELECT COUNT(argument) FROM argpoll WHERE argument=?" arg-id]
            (second (first (first res1)))),
   :value (jdbc/with-query-results
            res1 ["SELECT AVG(opinion) FROM argpoll WHERE argument=?" arg-id]
            (second (first (first res1))))})

(defn list-argument-poll
  "Returns a sequence of maps representing the argument poll table in the database"
  []
  (jdbc/with-query-results
    res1 ["SELECT * FROM argpoll"]
    (doall res1)))

(defn update-argument-poll
  "database string map -> boolean
   Updates the votes of a user.  The map is
   from argument ids to numbers in the range of 0.0 to 1.0."
  [arg]
  {:pre [(string? (:id arg))
         (map? (:votes arg))
         (every? (fn [x]
                   (and (number? x)
                        (<= 0.0 x 1.0)))
                 (vals (:votes arg)))]}
  (let [{:keys [votes id]} arg]
    (doseq [[argument opinion] votes]
      (jdbc/update-or-insert-values
       :argpoll
       ["userid=? AND argument=?" (str id) (name argument)]
       {:userid id,
        :argument (name argument)
        :opinion opinion})))
  true)

(defn delete-argument-poll
  "Deletes all votes for the given argument id."
  [arg-id]
  {:pre [(integer? arg-id)]}
  (jdbc/delete-rows :argpoll ["argument=?" arg-id])
  true)

(defn assume
  "Assumes the given statements to be true in the database, unless they have
   been assumed to be false, in which case their status is left unchanged."
  [stmts]
  {:pre [(every? literal? stmts)]}
  (doseq [stmt stmts]
    (let [id (get-statement (literal-atom stmt)),
          s  (read-statement id)]
      (cond
        (nil? (:weight s)) ; stated
        (update-statement id {:weight (if (literal-pos? stmt) 0.75 0.25)}),
        (and (literal-pos? stmt)
             (> (:weight s) 0)
             (<= (:weight s) 0.25))   ; assumed false
        (update-statement id {:weight 0.5}),  ; question
        (and (literal-neg? stmt)
             (> (:weight s) 0.5)
             (<= (:weight s) 0.75))  ; assumed true
        (update-statement id {:weight 0.5}))))) ; question

