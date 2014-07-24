;; Copyright (c) 2011 Fraunhofer Gesellschaft
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns ^{:doc  "Functions for exporting argument databases to argument graphs."}
  carneades.database.export
  (:require
        [carneades.engine.argument-graph :as ag]
        [carneades.database.db :as db]
        [carneades.database.argument-graph :as ag-db]
        [clojure.java.jdbc.deprecated :as jdbc]))

(defn export-to-argument-graph
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
          (ag/enter-statements (ag-db/list-statements))
          (ag/enter-arguments (ag-db/list-arguments))
          (ag/enter-references (ag-db/list-metadata))
          (ag/enter-namespaces (ag-db/list-namespaces))
          (assoc :header (ag-db/read-metadata 1))))))
        
