(ns carneades.web.modules.lican.legal-profiles-db
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
  [project user password]
  (let [conn (db/make-connection project db-name user password
                                 :create true)]
    (with-connection conn
      (create (table :profiles
                     (varchar :title 255)
                     (integer :id :auto-inc :primary-key))))))

(defn set-default-connection
  [project user password]
  (let [conn (db/make-connection project db-name user password)]
    (default-connection conn)))

(defn create-profile
  [profile]
  (insert profiles
          (values profile)))

(defn read-profiles
  []
  )

(defn read-profile
  [id]
  (select profiles
          (where {:id id})))

(defn update-profile
  [id update])

(defn delete-profile
  [id])
