;; Copyright (c) 2014 Fraunhofer Gesellschaft
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns ^{:doc "Administration of the different databases."}
  carneades.admin.db
  (:require [carneades.database.db :as db]
            [carneades.engine.utils :as f]
            [carneades.database.legal-profile :as lp]
            [carneades.engine.dublin-core :as dc]
            [carneades.database.argument-graph :as ag-db]))

(defn create-project-dbs
  [project username password]
  (when-not (f/exists? (db/dbfilename project "legal-profiles"))
    (lp/create-db project username password))
  (when-not (f/exists? (db/dbfilename project "main"))
    (ag-db/create-argument-database project "main"
                                    username password
                                    (dc/make-metadata
                                     :title "Default empty database"))))
