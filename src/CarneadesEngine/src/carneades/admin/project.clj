;; Copyright (c) 2014 Fraunhofer Gesellschaft
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns ^{:doc "Administration of the projects."}
  carneades.admin.project
  (:require [carneades.admin.db :as db]
            [carneades.project.fs :as pfs]))

(def default-properties
  {:title "A title"
   :schemes "walton_schemes"
   :description {:en "A description."}})

(defn create-project
  "Creates a new project, its files and databases."
  [name username password]
  (pfs/create-project-files name)
  (db/create-project-dbs name username password))
