;;; Copyright (c) 2011 Fraunhofer Gesellschaft
;;; Licensed under the EUPL V.1.1

(ns ^{:doc  "Functions for exporting rgument databases to argument graphs."}
  carneades.database.export
  (:use [carneades.engine.argument-graph :as ag]
        [carneades.database.db :as db])
  (:require
        [clojure.java.jdbc :as jdbc]))

(defn export-argument-graph
  "database-connection -> argument-graph
   Exports all the statement nodes, argument nodes, references and namespaces in 
   an argument database to an argument graph. The statement nodes and argument
   nodes of the argument graph may be assigned new ids in the graph.
   Returns the argument graph."
  [db]
  (jdbc/with-connection 
    db
    (jdbc/transaction
      (-> (ag/make-argument-graph)
          (ag/enter-statements (db/list-statements))
          (ag/enter-arguments (db/list-arguments))
          (ag/enter-references (db/list-metadata))
          (ag/enter-namespaces (db/list-namespaces))
          (assoc :header (db/read-metadata 1))
          ))))
        
