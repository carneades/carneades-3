;; Copyright (c) 2014 Fraunhofer Gesellschaft
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns ^{:doc "Database management of the legal profiles."}
  carneades.database.legal-profile
  (:require [carneades.engine.utils :refer [unserialize-atom serialize-atom]]
            [carneades.database.db :as db]
            [lobos.core :refer [create]]
            [lobos.schema :as schema :refer [table varchar integer]]
            [lobos.connectivity :refer [with-connection]]
            [korma.core :refer :all :exclude [table]]
            [korma.db :refer :all :exclude [create-db]]))

(def db-name "legal-profiles")

(defentity profiles
  (entity-fields :id :title))

(defentity rules
  (entity-fields :id :ruleid :value)
  (has-many profiles))

(defn create-db
  "Create the database."
  [project user password]
  (let [conn (db/make-connection project db-name user password
                                 :create true)]
    (with-connection conn
      (create (table :metadata
                     (integer :id :auto-inc :primary-key :unique)
                     (varchar :key '(MAX) :unique)
                     (varchar :contributor '(MAX))
                     (varchar :coverage '(MAX))
                     (varchar :creator '(MAX))
                     (varchar :date '(MAX))
                     ;; (:description)
                     (varchar :format '(MAX))    
                     (varchar :identifier '(MAX))
                     (varchar :language '(MAX))
                     (varchar :publisher '(MAX))
                     (varchar :relation '(MAX))
                     (varchar :rights '(MAX))
                     (varchar :source '(MAX))
                     (varchar :subject '(MAX))
                     (varchar :title '(MAX))
                     (varchar :type '(MAX))))
      (create (table :profiles
                     (integer :id :auto-inc :primary-key :unique)
                     (integer :metadatum [:refer :metadata :id :on-delete :set-null])))
      (create (table :rules
                     (integer :id :auto-inc :primary-key :unique)
                     (varchar :ruleid 255)
                     ;; TRUE, FALSE or UNKNOWN that is in, out, undecided
                     (schema/boolean :value)
                     (integer :profile [:refer :profiles :id :on-delete :set-null] :not-null))))))

(defn set-default-connection
  "Set the default connection for the database."
  [project user password]
  (let [conn (db/make-connection project db-name user password)]
    (default-connection conn)))

(defn create-profile
  "Create a profile in the database."
  [profile]
  (first (vals (insert profiles
                 (values profile)))))

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
  (update profiles
          (set-fields change)
          (where {:id [= id]})))

(defn delete-profile
  "Delete a profile in the database."
  [id]
  (delete rules (where {:profile [= id]}))
  (delete profiles (where {:id [= id]})))

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
      (dissoc :profile)))

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

(defn pack-profile+
  [profile+]
  (dissoc profile+ :rules))

(defn create-profile+
  "Create a profile with its associated rules in the database."
  [profile+]
  (let [profile' (pack-profile+ profile+)
        p (first (vals (insert profiles
                               (values profile'))))]
    (doseq [rule (:rules profile+)]
      (create-rule p rule))
    p))

(defn read-profile+
  "Read a profile and its associated rules in the database."
  [id]
  (assoc (read-profile id)
    :rules (map unpack-rule
                (select rules
                    (where {:profile [= id]})))))
