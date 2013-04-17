;;; Copyright (c) 2011 Fraunhofer Gesellschaft
;;; Licensed under the EUPL V.1.1

(ns ^{:doc  "Functions for importing argument graphs into a database."}
  carneades.database.import
  (:use carneades.engine.argument
        carneades.engine.statement
        carneades.engine.dublin-core
        carneades.database.db)
  (:require [clojure.java.jdbc :as jdbc]
            [carneades.database.argument-graph :as ag]))

(defn import-from-argument-graph
  "database-connection argument-graph boolean -> boolean
   Imports all the statement nodes, argument nodes, references and namespaces of the 
   argument graph into the database. Optionally, the metadata record describing 
   the database is updated with the information in the header of the argument graph.
   Returns true if the import is successful."
  [db arg-graph update-header]
  (jdbc/with-connection 
    db
    (jdbc/transaction
      
      ; Statements
     (doseq [sn (vals (:statement-nodes arg-graph))]
        (ag/create-statement (map->statement sn)))
      
      ; Arguments
      (doseq [an (vals (:argument-nodes arg-graph))]
        (let [premises (map (fn [p]
                              (let [sn (get (:statement-nodes arg-graph)
                                            (:statement p))]
                                (assoc p 
                                  :statement 
                                  (:id sn))))
                            (:premises an))]
         (ag/create-argument 
          (assoc (map->argument an)
            :conclusion  (:id (get (:statement-nodes arg-graph)
                                   (literal-atom (:conclusion an))))
            :premises premises))))
      
      ; References
      (doseq [md (:references arg-graph)]
        (ag/create-metadata (assoc (second md) :key (first md))))
      
      ; Namespaces
      (doseq [ns (:namespaces arg-graph)]
        (ag/create-namespace {:prefix (first ns) :uri (second ns)}))
      
      ; Header
      (when (and update-header (:header arg-graph))
        (ag/update-metadata 1 (:header arg-graph)))))
  true)


