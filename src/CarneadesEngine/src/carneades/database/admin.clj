;; Copyright (c) 2014 Fraunhofer Gesellschaft
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns ^{:doc "Administration of the different databases."}
  carneades.database.admin
  (:require [carneades.database.db :as db]
            [carneades.engine.utils :as f]
            [carneades.database.legal-profile :as lp]
            [carneades.engine.dublin-core :as dc]
            [carneades.database.argument-graph :as ag-db]))

(defn create-missing-dbs
  [project username password]
  (when-not (f/exists? (db/dbfilename "legal-profiles"))
    (lp/create-db project username password))
  (when-not (f/exists? (db/dbfilename "main"))
    (let [ag (ag/make-argument-graph 
              :header (dc/make-metadata 
                       :title "Default empty database"))]
      (ag-db/create-argument-database project "main"
                                      username password
                                      (dc/make-metadata
                                       :title "Default empty database")))))
