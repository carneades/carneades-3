;;; Copyright (c) 2013 Fraunhofer Gesellschaft
;;; Licensed under the EUPL V.1.1

(ns ^{:doc "Utility to evaluate an argument graph stored in the database."}
    carneades.database.evaluation
  (:use [carneades.database.export :only [export-to-argument-graph]]
        [carneades.engine.argument-evaluation :only [evaluate]]
        ;; there is a problem with the aspic evaluation
        ;; [carneades.engine.aspic :only [aspic-grounded]]
        [carneades.engine.caes :only [caes]])
  (:require [carneades.database.db :as db]
            [carneades.database.argument-graph :as ag-db]))

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
