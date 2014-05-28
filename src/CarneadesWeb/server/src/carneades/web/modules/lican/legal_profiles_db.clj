;; Copyright (c) 2014 Fraunhofer Gesellschaft
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns ^{:doc "Database management of the legal profiles."}
  carneades.web.modules.lican.legal-profiles-db
  (:require [carneades.database.db :as db]
            [lobos.core :refer [create]]
            [lobos.schema :refer [table varchar integer]]
            [lobos.connectivity :refer [with-connection]]
            [korma.core :refer :all :exclude [table]]
            [korma.db :refer :all :exclude [create-db]]))

(def db-name "legal-profiles")

(defentity profiles
  (entity-fields :id :title))

(defn create-db
  "Create the database."
  [project user password]
  (let [conn (db/make-connection project db-name user password
                                 :create true)]
    (with-connection conn
      (create (table :profiles
                     (varchar :title 255)
                     (integer :id :auto-inc :primary-key))))))

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

(defn update-profile
  "Update a profile in the database."
  [id change]
  (update profiles
          (set-fields change)
          (where {:id [= id]})))

(defn delete-profile
  "Delete a profile in the database."
  [id]
  (delete profiles (where {:id [= id]})))
