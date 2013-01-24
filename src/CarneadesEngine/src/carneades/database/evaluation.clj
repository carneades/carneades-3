;;; Copyright (c) 2013 Fraunhofer Gesellschaft
;;; Licensed under the EUPL V.1.1

(ns ^{:doc "Utility to evaluate an argument graph stored in the database."}
    carneades.database.evaluation
  (:use [carneades.database.export :only [export-to-argument-graph]]
        [carneades.engine.argument-evaluation :only [evaluate]]
        [carneades.engine.aspic :only [aspic-grounded]])
  (:require [carneades.database.db :as db]))

(defn evaluate-graph
  "Evalutes a graph stored in the database."
  [dbname username password]
  (let [dbconn (db/make-database-connection dbname username password)
        ag1 (export-to-argument-graph dbconn)
        ag2 (evaluate aspic-grounded ag1)]
    (db/with-db dbconn
      (doseq [sn (vals (:statement-nodes ag2))]
        (db/update-statement (str (:id sn))
                             {:value (:value sn)}))
      (doseq [an (vals (:argument-nodes ag2))]
        (db/update-argument (str (:id an))
                            {:value (:value an)}))
      {:body             true})))