(ns carneades.web.modules.lican.legal-profiles-db
  (:require [carneades.database.db :as db]
            [lobos.core :refer [create]]
            [lobos.schema :refer [table varchar]]
            [lobos.connectivity :refer [with-connection]]))

(def db-name "legal-profiles")

(defn create-legal-profiles-db
  [project user password]
  (let [conn (db/make-connection project db-name user password
                                 :create true)]
    (with-connection conn
      (create (table :profiles (varchar :title 255))))))
