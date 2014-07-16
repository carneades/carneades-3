;; Copyright (c) 2010 Fraunhofer Gesellschaft
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns ^{:doc "Functions for exporting argument graphs to JSON using the Argument Interchange Format (AIF)."}
    carneades.serialization.aif.export
<<<<<<< HEAD
  (:require [carneades.engine.statement :as stmt]
            [carneades.engine.argument-graph :as ag]
            [carneades.engine.uuid :as id]
            [cheshire.core :as json]
            [clojure.string :as str]
            [clojure.set :as set]))
=======
  (:use carneades.engine.statement
        carneades.engine.argument-graph
        carneades.engine.uuid)
  (:require [cheshire.core :as json]
            [clojure.string :as str]))
>>>>>>> 96967030f1a4a3132beec4982f49e1ac1df75f2c

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
<<<<<<< HEAD
  "ArgumentGraph -> (Map Symbol Statement) 
   Make a statement for the complement of each statement node of an argument graph.
   Returns a map from the ids of statement nodes in the argument graph to their complements."
  [ag]
  (reduce (fn [m sn]
            (assoc m (:id sn)  
                   (stmt/make-statement :id (id/make-urn-symbol)
                                        :text (negate-text (:text sn)))))
          {}
          (vals (:statement-nodes ag))))

(defrecord Conflict
    [id       ;; URN symbol
     stmts])  ;; URNs of the conflicting Statements
  
(defn- make-conflicts
  "(Map Symbol Statement) (Seq Symbol) -> (Seq Conflict)
   Returns conflicts for statements with keys of the complements table
   if the ids of *both* the key and its value are in the given sequence."  
  [complements ids]
  (mapcat (fn [k] (let [cid (:id (get complements k))]
                    (if (and (some (fn [id] (= id k))  ids)
                             (some (fn [id] (= id cid))  ids))
                      [(Conflict. (id/make-urn-symbol)
                                  [k cid])]
                      [])))
          (keys complements)))

(defn- make-aif-node
  "keyword (U Statement StatementNode ArgumentNode) -> AifNode"
  [lang x]
  (cond (or (stmt/statement? x) (ag/statement-node? x))
        {:nodeID (id/uuid->string (id/symbol->uuid (:id x))),
         :text (lang (:text x)),
         :type "I"}

        (ag/argument-node? x) 
        {:nodeID (id/uuid->string (id/symbol->uuid (:id x))),
         :text "RA",
         :type "RA"}

        (instance? Conflict x)
        {:nodeID (id/uuid->string (id/symbol->uuid (:id x)))
         :text "CA"
         :type "CA"}))

(defn- make-aif-nodes
  "Keyword (Seq (U Statement StatementNode ArgumentNode)) -> (Seq AifNode)"
  [lang l]
  (map (fn [x] (make-aif-node lang x)) l))

(defn- argnode->edges
  "(Map Symbol Statement) ArgumentNode -> (Seq AifEdge)"
  [complements an]
  (conj (map (fn [premise] 
               {:edgeID (id/uuid->string (id/make-uuid)),
                ;; if the premise is not positive, make an edge from 
                ;; the complement of the statement of the premise
                :fromID (id/uuid->string (id/symbol->uuid 
=======
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
>>>>>>> 96967030f1a4a3132beec4982f49e1ac1df75f2c
                                       (if (:positive premise)
                                         (:statement premise)
                                         (:id (get complements 
                                                   (:statement premise)))))) 
<<<<<<< HEAD
                :toID (id/uuid->string (id/symbol->uuid (:id an)))})
             (:premises an))
        ;; create an aif-edge from the argument node to its conclusion
        ;; If the argument is a con argument, use the complement of the conclusion.
        {:edgeID (id/uuid->string (id/make-uuid))
         :fromID (id/uuid->string (id/symbol->uuid (:id an)))
         :toID (id/uuid->string (id/symbol->uuid 
=======
                :toID (uuid->string (symbol->uuid (:id an)))})
             (:premises an))
        ;; create an aif-edge from the argument node to its conclusion
        ;; If the argument is a con argument, use the complement of the conclusion.
        {:edgeID (uuid->string (make-uuid))
         :fromID (uuid->string (symbol->uuid (:id an)))
         :toID (uuid->string (symbol->uuid 
>>>>>>> 96967030f1a4a3132beec4982f49e1ac1df75f2c
                              (if (:pro an)
                                (:conclusion an)
                                (:id (get complements (:conclusion an)))))) }))

<<<<<<< HEAD
(defn- make-ra-edges
  "(Map Symbol Statement) (Seq ArgumentNode) -> (Seq AifEdge)"
  [complements arg-nodes]
  (mapcat (fn [an] (argnode->edges complements an))
          arg-nodes))

(defn- used-statements
  "(Seq StatementNode)
   (Map Symbol Statment) 
   (Seq ArgumentNode) 
   -> (Set (U Statement StatementNode))
   Find statements which are used as a premise or conclusion of an argument."
  [sns complements ans] 
  (mapcat (fn [sn] 
            (reduce (fn [s an] 
                      (set/union s
                              (if (or (and (:pro an)
                                           (= (:id sn)
                                              (:conclusion an)))
                                      (some (fn [p] 
                                              (and (:positive p)
                                                    (= (:id sn)
                                                       (:statement p))))
                                            (:premises an)))
                                #{sn}
                                #{})
                              (if (or (and (not (:pro an))
                                           (= (:id sn)
                                              (:conclusion an)))
                                      (some (fn [p] 
                                              (and (not (:positive p))
                                                   (= (:id sn)
                                                      (:statement p))))
                                            (:premises an)))
                                #{(get complements (:id sn))}
                                #{})))
                    #{}
                    ans))
          sns))

(defn- make-ca-edges
  "(Seq Conflict) -> (Seq Map)
   Make maps representing CA edges for each conlict."
  [conflicts]
  (mapcat (fn [conflict] 
            (let [premise-id (id/uuid->string 
                              (id/symbol->uuid 
                               (get (:stmts conflict) 0)))
                  ca-id (id/uuid->string 
                         (id/symbol->uuid (:id conflict)))
                  conclusion-id  (id/uuid->string 
                                  (id/symbol->uuid 
                                   (get (:stmts conflict) 1)))]
              (vector {:edgeID (id/uuid->string (id/make-uuid))
                       :fromID premise-id
                       :toID  ca-id}
                      {:edgeID (id/uuid->string (id/make-uuid))
                       :fromID ca-id
                       :toID conclusion-id})))
          conflicts))

(defn argument-graph->aif
  "ArgumentGraph Keyword -> String"
  [ag lang]
  (let [complements (make-complements ag)
        used-stmts (used-statements (vals (:statement-nodes ag))
                                    complements 
                                    (vals (:argument-nodes ag)))
        conflicts (make-conflicts complements (map :id used-stmts))]
    (json/encode 
     {:nodes (make-aif-nodes lang 
                             (concat used-stmts
                                     (vals (:argument-nodes ag))
                                     conflicts))
      :edges (concat (make-ra-edges complements (vals (:argument-nodes ag)))
                     (make-ca-edges conflicts))})))

=======
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

  
>>>>>>> 96967030f1a4a3132beec4982f49e1ac1df75f2c
