;; Copyright (c) 2013 Fraunhofer Gesellschaft
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns ^{:doc "Utility to evaluate an argument graph stored in the database."}
    carneades.database.evaluation
    (:use [carneades.database.export :only [export-to-argument-graph export-to-argument-graph']]
        [carneades.engine.argument-evaluation :only [evaluate]]
        ;; there is a problem with the aspic evaluation
        ;; [carneades.engine.aspic :only [aspic-grounded]]
        [carneades.engine.caes :only [caes]])
  (:require [carneades.database.db :as db]
            [carneades.database.argument-graph :as ag-db]))

(defn evaluate-graph'
  []
  (let [ag1 (export-to-argument-graph')
        ag2 (evaluate caes ag1)]
    (doseq [sn (vals (:statement-nodes ag2))]
      (ag-db/update-statement (str (:id sn))
                              {:value (:value sn)}))
    (doseq [an (vals (:argument-nodes ag2))]
      (ag-db/update-argument (str (:id an))
                             {:value (:value an)}))))

(defn evaluate-graph
  "Evalutes a graph stored in the database."
  [project dbname username password]
  (let [dbconn (db/make-connection project dbname username password)
        ag1 (export-to-argument-graph dbconn)
        ag2 (evaluate caes ag1)]
    (db/with-db dbconn
      (doseq [sn (vals (:statement-nodes ag2))]
        (ag-db/update-statement (str (:id sn))
                                {:value (:value sn)}))
      (doseq [an (vals (:argument-nodes ag2))]
        (ag-db/update-argument (str (:id an))
                               {:value (:value an)}))
      {:body             true})))
