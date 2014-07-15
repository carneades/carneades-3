;; Copyright (c) 2010 Fraunhofer Gesellschaft
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns ^{:doc "Functions for exporting argument graphs to JSON using the Argument Interchange Format (AIF)."}
    carneades.serialization.aif.export
  (:use carneades.engine.statement
        carneades.engine.argument-graph
        carneades.engine.uuid)
  (:require [cheshire.core :as json]
            [clojure.string :as str]))

(defn- negate-text
  "(Map language string) -> (Map language string)
   Negate the text for each langauge in the map."
  [m1]
  (reduce (fn [m2 k] 
            (assoc m2 k 
                   (condp = k 
                     :en (str "It is not the case that: " (get m1 k))
                     :nl (str "Het is niet zo dat: " (get m1 k))
                     :fr (str "Ce n'est pas le cas que: " (get m1 k))
                     :de (str "Es ist nicht der Fall daß: " (get m1 k))
                     :it (str "Non è il caso che: " (get m1 k))
                     :sp (str "No es el caso que: " (get m1 k)))))
          m1
          (keys m1)))
                    
(defn- make-complements
  "ArgumentGraph -> (Map Symbol StatementNode) 
   Make a statement node for the complement of each statement node of an argument graph.
   Returns a map from the ids of statement nodes in the argument graph to their complements."
  [ag]
  (reduce (fn [m sn]
            (assoc m (:id sn) 
                   (make-statement-node 
                    (make-statement :id (make-urn-symbol))
                                    :text (negate-text (:text sn)))))
          {}
          (:statement-nodes ag)))

(defn- make-aif-node
  "keyword (U StatementNode ArgumentNode) -> AifNode"
  [lang node]
  (cond (statement-node? node) 
        {:nodeID (uuid->string (symbol->uuid (:id node))),
         :text (lang (:text node)),
         :type "I"}

        (argument-node? node) 
        {:nodeID (uuid->string (symbol->uuid (:id node))),
         :text "RA",
         :type "RA"}))

(defn- make-aif-nodes
  "Keyword (Seq (U StatementNode ArgumentNode)) -> (Seq AifNode)"
  [lang nodes]
  (map (fn [node] (make-aif-node lang node)) 
       nodes))

(defn- argnode->edges
  "(Map Symbol StatementNode) ArgumentNode -> (Seq AifEdge)"
  [complements an]
  (conj (map (fn [premise] 
               {:edgeID (uuid->string (make-uuid)),
                ;; if the premise is not positive, make an edge from 
                ;; the complement of the statement of the premise
                :fromID (uuid->string (symbol->uuid 
                                       (if (:positive premise)
                                         (:statement premise)
                                         (:id (get complements 
                                                   (:statement premise)))))) 
                :toID (uuid->string (symbol->uuid (:id an)))})
             (:premises an))
        ;; create an aif-edge from the argument node to its conclusion
        ;; If the argument is a con argument, use the complement of the conclusion.
        {:edgeID (uuid->string (make-uuid))
         :fromID (uuid->string (symbol->uuid (:id an)))
         :toID (uuid->string (symbol->uuid 
                              (if (:pro an)
                                (:conclusion an)
                                (:id (get complements (:conclusion an)))))) }))

(defn- make-aif-edges
  "(Map Symbol StatementNode) (Seq ArgumentNode) -> (Seq AifEdge)"
  [complements arg-nodes]
  (mapcat (fn [an] (argnode->edges complements an))
          arg-nodes))
  
(defn argument-graph->aif
  "ArgumentGraph Keyword -> String"
  [ag lang]
  (let [complements (make-complements (:statement-nodes ag))]
    (json/encode 
     {:nodes (make-aif-nodes lang 
                             (concat (vals (:statement-nodes ag))
                                     (vals complements)
                                     (vals (:argument-nodes ag))))
      :edges (make-aif-edges complements (vals (:argument-nodes ag)))})))

  
