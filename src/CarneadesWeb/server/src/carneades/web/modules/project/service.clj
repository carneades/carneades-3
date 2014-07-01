;; Copyright (c) 2014 Fraunhofer Gesellschaft
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns ^{:doc "Functions from the old webservice"}
  carneades.web.modules.project.service
  (:require 
   [carneades.database.db :as db]
   [carneades.web.pack :as p]
   [carneades.database.argument-graph :as ag-db]))

(defn get-statement
  [project db id]
  (let [dbconn (db/make-connection project db "guest" "")]
    (db/with-db dbconn
      (p/pack-statement (ag-db/read-statement id)))))
