;; Copyright (c) 2013 Fraunhofer Gesellschaft
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

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

(defn get-project-properties
  [id state]
  (merge (get-in (deref state) [:projects-data id :properties])
         {:id id}))

(defn get-project-theories
  [id state]
  (get-in (deref state) [:projects-data id :available-theories]))

(defn get-project-documents
  [id state]
  (get-in (deref state) [:projects-data id :documents]))
