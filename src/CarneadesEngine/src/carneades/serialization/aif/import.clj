;; Copyright (c) 2014 Fraunhofer Gesellschaft
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns ^{:doc "Functions for importing argument graphs from JSON using 
            the Argument Interchange Format (AIF)."}
  carneades.serialization.aif.import
  (:require [carneades.engine.statement :as stmt]
            [carneades.engine.argument :as arg]
            [carneades.engine.argument-graph :as ag]
            ;; [carneades.engine.uuid :as id]
            [cheshire.core :as json]
            ;; [clojure.string :as str]
            ;; [clojure.set :as set]
            ;; [taoensso.timbre :as timbre :refer [debug info spy]]
            ))

(defn- parse-i-nodes 
  "(Map Keyword Vector) Keyword -> (Map String Statement)
   Takes a Clojure map representation of an AIF file, serialized using JSON,
   and translates the I-Nodes into Carneades statements. 
   A map from AIF ids to statements is returned."
  [aif lang]
  (reduce (fn [m node]
            (if (= (get node "type") "I")
              (let [stmt (stmt/make-statement 
                          :text {lang (get node "text")})]
                (assoc m (get node "nodeID") stmt))
              m))
          {}
          (get aif "nodes")))

(defn- ca-node-conclusion
  "String (Map Keyword Vector) (Map String Statement) -> Statement
   Returns the Carneades statement for the conclusion of 
   a CA-node in an AIF file."
   [ca-node-id aif stmts]
   (->> (get aif "edges")
        (filter (fn [edge] (= (get edge "fromID") ca-node-id)))
        (first)
        ((fn [e] (get e "toID")))
        (get stmts)))

(defn- ca-node-premise
  "String (Map Keyword Vector) (Map String Statement) -> Statement
   Returns the Carneades statement for the premise of 
   a CA-node in an AIF file. It is assumed that each CA node has 
   exactly one premise."
   [ca-node-id aif stmts]
   (->> (get aif "edges")
        (filter (fn [edge] (= (get edge "toID") ca-node-id)))
        (first)
        ((fn [e] (get e "fromID")))
        (get stmts)))

(defn- parse-ca-nodes
  "(Map Symbol Vector) (Map String Statement) -> (Map Symbol Statement) 
    Takes a Clojure map representation of an AIF file, serialized
    using JSON, and a map from AIF ids (strings) to Carneades
    statements and parses the CA-Nodes to produce a map from the
    Carneades id of a statement to the Carneades statement
    representing its complement, if one exists in the AIF file. It is
    assumed that CA-nodes link a statement, P, to the negation of the
    statement, (not P), and that the reverse link, from the (not P) to
    P is not explicitly represented in the AIF file. Since only
    positive statements are explicitly represented in Carneades
    graphs, the map returned is from the negated statements to their
    positive complements, to enable the positive complement to be
    looked up when constructing the argument graph."  
  [aif stmts]
  (reduce (fn [m node]
            (if (= (get node "type") "CA") 
              (assoc m 
                (:id (ca-node-conclusion (get node "nodeID") aif stmts))
                (ca-node-premise (get node "nodeID") aif stmts))
              m))
          {}
          (get aif "nodes")))

(defn- ra-node-conclusion 
  "String (Map Keyword Vector) (Map Symbol Statement) -> Statement
   Returns the Carneades statement for the conclusion of an RA-node in 
   the Clojure map representation of an AIF JSON file."
  [ra-node-id aif stmts]
  (->>  (get aif "edges")
        (filter (fn [edge] (= (get edge "fromID") ra-node-id)))
        (first)
        ((fn [e] (get e "toID")))
        (get stmts)))

(defn- ra-node-premises 
  "String (Map Keyword Vector) (Map Symbol Statement) -> (Seq Statement)
   Returns a vector of Carneades statements for the premises of an RA-node in 
   the Clojure map representation of an AIF JSON file."
  [ra-node-id aif stmts]
  (->> (get aif "edges")
       (filter (fn [e] (= (get e "toID") ra-node-id)))
       (map (fn [e] (get e "fromID")))
       (map (fn [id] (get stmts id)))))

(defn- parse-ra-nodes 
  "(Map Keyword Vector) (Map String Statement) (Map Symbol Statement) -> (Seq Argument)"
  [aif stmts complements]
  (reduce (fn [args node]
            (if (= (get node "type") "RA") 
              (let [conclusion (ra-node-conclusion (get node "nodeID") aif stmts)
                    premises (ra-node-premises (get node "nodeID") aif stmts) ] 
                (conj args (arg/make-argument 
                            :pro (not (contains? complements (:id conclusion)))
                            :conclusion (if (contains? complements (:id conclusion))
                                          (get complements (:id conclusion))
                                          conclusion)
                            :premises (map (fn [p] (arg/make-premise 
                                                    :positive (not (contains? complements (:id p)))
                                                    :statement (if (contains? complements (:id p))
                                                                 (get complements (:id p))
                                                                 p))) 
                                           premises))))
              args))
          []
          (get aif "nodes")))
                          
(defn aif->argument-graph
  "Reader Keyword -> ArgumentGraph
   The lang keyword gives the language of the AIF file. It is assumed
   that the text of all nodes in an AIF file is in the same language."
  [rdr lang]
  (let [aif (json/parse-stream rdr)
        stmts (parse-i-nodes aif lang)
        complements (parse-ca-nodes aif stmts)
        args (parse-ra-nodes aif stmts complements)]
    (-> (ag/make-argument-graph)
        (ag/enter-arguments args))))


