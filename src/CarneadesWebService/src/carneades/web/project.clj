(ns carneades.web.project
  (:use [carneades.engine.utils :only [exists?]])
  (:require [carneades.database.db :as db]
            [carneades.database.case :as case]
            [carneades.project.admin :as project]))

(defn init-debate-db
  [project]
  (when (not (exists? (db/dbfilename project "debates")))
    (case/create-debate-database project "debates" "root" "pw1")
    (db/with-db (db/make-connection project "debates" "root" "pw1"
                                    :create true)
      (case/create-debate {:title "Debates"
                           :public true
                           :id "main"}))))

(defn init-projects-data!
  "Returns the project data and creates debate databases if missing."
  []
  (reduce (fn [m project]
            (init-debate-db project)
            (assoc m project (project/load-project project)))
          {}
   (project/list-projects)))
