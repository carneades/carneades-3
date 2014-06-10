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
      (create (table :profiles
                     (integer :id :auto-inc :primary-key :unique)
                     (varchar :title 255)))
      (create (table :rules
                     (integer :id :auto-inc :primary-key :unique)
                     (varchar :ruleid 255)
                     ;; TRUE, FALSE or UNKNOWN that is in, out, undecided
                     (schema/boolean :value)
                     (integer :profile [:refer :profiles :id :on-delete :set-null]))))))

(defn set-default-connection
  "Set the default connection for the database."
  [project user password]
  (let [conn (db/make-connection project db-name user password)]
    (default-connection conn)))

(defn create-profile
  "Create a profile in the database."
  [profile]
  (insert profiles
          (values profile)))

(defn read-profiles
  "Read all profiles in the database."
  []
  (select profiles))

(defn read-profile
  "Read a profile in the database."
  [id]
  (select profiles
          (where {:id id})))

(defn read-profile+
  "Read a profile and its associated rules in the database."
  [id])

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

(defn create-rule
  "Create a rule in the database."
  [profileid ruleid value]
  (insert rules
          (values {:profile profileid
                   :ruleid (serialize-atom ruleid)
                   :value (cond (>= value 0.99) true
                                (<= value 0.01) false
                                :else nil)})))

(defn read-rule
  "Read a rule in the database."
  [id]
  (when-let [rule (first
                   (select rules
                           (where {:id [= id]})))]
    (-> rule
        (update-in [:ruleid] unserialize-atom)
        (assoc :value (condp = (:value rule)
                        true 1.0
                        false 0.0
                        0.5)))))

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
