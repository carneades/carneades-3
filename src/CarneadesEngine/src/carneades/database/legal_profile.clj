;; Copyright (c) 2014 Fraunhofer Gesellschaft
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns ^{:doc "Database management of the legal profiles."}
  carneades.database.legal-profile
  (:require [carneades.engine.utils :refer [unserialize-atom serialize-atom]]
            [clojure.java.jdbc :as jdbc]
            [carneades.database.db :as db]
            [korma.core :refer :all :exclude [table]]
            [korma.db :refer :all :exclude [create-db with-db] :as k]
            [sqlingvo.core :as s]
            [taoensso.timbre :as timbre :refer [debug info spy]]))

(def db-name "legal-profiles")

(defentity metadata
  (entity-fields :key
                 :contributor
                 :coverage
                 :creator
                 :date
                 :format
                 :identifier
                 :language
                 :publisher
                 :relation
                 :rights
                 :source
                 :subject
                 :title
                 :type
                 ))

(defentity profiles
  (entity-fields :id :default :metadatum)
  (has-one metadata))

(defentity rules
  (entity-fields :id :ruleid :value)
  (has-many profiles))

(defn create-db
  "Create the database."
  [project user password]
  (let [conn (db/make-connection project db-name user password
                                 :create true)]
    (jdbc/with-db-transaction [conn conn]
      (jdbc/execute!
       conn
       (s/sql (s/create-table
               :metadata
               (s/column :id :serial :primary-key? true)
               (s/column :key :varchar :unique? true)
               (s/column :contributor :varchar)
               (s/column :coverage :varchar)
               (s/column :creator :varchar)               
               (s/column :date :varchar)
               (s/column :format :varchar)
               (s/column :identifier :varchar)
               (s/column :publisher :varchar)
               (s/column :language :varchar)
               (s/column :relation :varchar)
               (s/column :rights :varchar)
               (s/column :source :varchar)
               (s/column :subject :varchar)
               (s/column :title :varchar)
               (s/column :type :varchar))))

      (jdbc/execute!
       conn
       (s/sql (s/create-table
               :profiles
               (s/column :id :serial :primary-key? true)
               (s/column :default :boolean :default 'false)
               (s/column :metadatum :integer :not-null? true :references :metadata/id))))

      (jdbc/execute!
       conn
       (s/sql (s/create-table
               :rules
               (s/column :id :serial :primary-key? true)
               (s/column :ruleid :varchar)
               (s/column :value :boolean) ;; TRUE, FALSE or UNKNOWN that is in, out, undecided
               (s/column :profile :integer :not-null? true :references :profiles/id)))))))

(defmacro with-db
  [project user password & body]
  `(let [conn# (db/make-connection ~project ~db-name ~user ~password)]
     (k/with-db conn# ~@body)))

(defn create-profile
  "Create a profile in the database."
  [profile]
  (transaction
   (let [id (first (vals (insert profiles
                                 (values profile))))]
     (when (:default profile)
       (update profiles
               (set-fields {:default false})
               (where {:id [not= id]})))
     id)))

(defn read-profiles
  "Read all profiles in the database."
  []
  (select profiles))

(defn read-profile
  "Read a profile in the database."
  [id]
  (first
   (select profiles
           (where {:id id}))))

(defn update-profile
  "Update a profile in the database."
  [id change]
  (when (= (:default change) false)
    (throw (ex-info "Invalid update. Cannot set the default property
           to false. Set the default property of another profile to
           true if you want to change this one."
                    {:update change})))
  (transaction
   (update profiles
           (set-fields change)
           (where {:id [= id]}))
   (when (:default change)
     (update profiles
             (set-fields {:default false})
             (where {:id [not= id]})))))

(defn delete-profile
  "Delete a profile in the database."
  [id]
  (transaction
   (let [profile (read-profile id)]
     (when (:default profile)
       (throw (ex-info "Deleting the default profile is forbidden."
                       {:profile profile}))))
   (delete rules (where {:profile [= id]}))
   (delete profiles (where {:id [= id]}))))

(defn pack-rule
  [rule]
  (-> rule
      (assoc :value
        (cond (>= (:value rule) 0.99) true
              (<= (:value rule) 0.01) false
              :else nil)
        :ruleid (serialize-atom (:ruleid rule)))
      (dissoc :id)))

(defn create-rule
  "Create a rule in the database."
  [profileid rule]
  (first (vals
          (insert rules
                  (values (merge (pack-rule rule)
                                 {:profile profileid}))))))

(defn unpack-rule
  [raw]
  (-> raw
      (update-in [:ruleid] unserialize-atom)
      (assoc :value (condp = (:value raw)
                      true 1.0
                      false 0.0
                      0.5))
      (dissoc :profile :id)))

(defn read-rule
  "Read a rule in the database."
  [id]
  (when-let [rule (first
                   (select rules
                           (where {:id [= id]})))]
    (unpack-rule rule)))

(defn update-rule
  "Update a rule in the database."
  [id change]
  (update rules
          (set-fields change)
          (where {:id [= id]})))

(defn update-rules
  "Update each rule in the [(id, update)] collection."
  [updates])

(defn delete-rule
  "Delete a rule in the database."
  [id]
  (delete rules (where {:id [= id]})))

(defn create-metadatum
  "Create a metadatum in the database."
  [metadatum]
  (first (vals
          (insert metadata
                  (values metadatum)))))

(defn read-metadatum
  "Read a metadatum in the database."
  [id]
  (first (select metadata
                 (where {:id [= id]}))))

(defn update-metadatum
  "Update a metadatum in the database."
  [id change]
  (update metadata
          (set-fields change)
          (where {:id [= id]})))

(defn delete-metadatum
  "Delete a metadatum."
  [id]
  (delete metadata
          (where {:id [= id]})))

(defn pack-profile+
  [profile+ metadatumid]
  (let [profile (-> profile+
                    (dissoc :rules)
                    (dissoc :metadata))]
    (if metadatumid
      (assoc profile :metadatum metadatumid)
      profile)))

(defn create-profile+
  "Create a profile with its associated rules and metadata in the database."
  [profile+]
  (spy profile+)
  (transaction
   (let [metadatumid (create-metadatum (:metadata profile+))
         profile' (pack-profile+ profile+ metadatumid)
         p (create-profile profile')]
     (doseq [rule (:rules profile+)]
       (create-rule p rule))
     p)))

(defn read-profile+
  "Read a profile and its associated rules and metadata in the database."
  [id]
  (transaction
   (when-let [profile (read-profile id)]
     (-> (spy profile)
         (assoc
             :rules (map unpack-rule
                         (select rules
                                 (where {:profile [= id]})))
             :metadata (spy (read-metadatum (:metadatum profile))))
         (dissoc :metadatum)))))

(defn read-profiles+
  "Read a profile with its associated rules and metadata in the database."
  []
  (transaction
   (map #(read-profile+ (:id %)) (select profiles (fields :id)))))

(defn update-profile+
  "Update a profile with its associated rules and metadata in the database."
  [id change]
  (transaction
   (let [change' (if (seq (:metadata change))
                   (let [oldmetadataid (:metadatum (read-profile id))
                         metadataid (create-metadatum (:metadata change))]
                     (when oldmetadataid
                       (delete-metadatum oldmetadataid))
                     (pack-profile+ change metadataid))
                   (pack-profile+ change nil))]
     (when (seq (:rules change))
       (do
         (delete rules (where {:profile [= id]}))
         (doseq [r (:rules change)]
           (create-rule id r))))
     (when-not (empty? change')
       (update-profile id change')))))
