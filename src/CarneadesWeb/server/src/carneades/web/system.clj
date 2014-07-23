;; Copyright (c) 2014 Fraunhofer Gesellschaft
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns carneades.web.system
  (:require [carneades.project.fs :as p]
            [carneades.admin.db :as dbadmin]))

(def state (atom nil))

(defn- init-projects-data!
  "Returns the project data and creates mandatory databases if missing."
  []
  (reduce (fn [m project]
            (dbadmin/create-project-dbs project "root" "pw1")
            (assoc m project (p/load-project project)))
          {}
          (p/list-projects)))

(defn- init-projects-data
  []
  {:projects (p/list-projects)
   :projects-data (init-projects-data!)})

(defn start
  []
  (reset! state (init-projects-data)))

(defn stop
  []
  )
