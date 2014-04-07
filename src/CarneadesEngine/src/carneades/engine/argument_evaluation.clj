;; Copyright (c) 2011 Fraunhofer Gesellschaft
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns ^{:doc "The protocol for argument evaluation structures.  An argument graph is evaluated
            by assigning values to its statement and argument nodes, where the values
            are real numbers in the range of 0.1 to 1.0. Argument evaluators also provide
            labelling functions for interpreting the numeric values by mapping them to symbols. 
            A standard set of labels is defined, with fixed interpretations, but argument 
            evaluators can extend this set of labels. Argument evaluators must assign
            a unique value and label to each statement and argument node of an argument
            graph.  If arguments are evaluated using a Dung argumentation framework, where
            multiple extentions and labellings are possible, depending on the chosen
            semantics, the result can be mapped to a unique labelling by choosing a
            credulous or skeptical variation of the semantics.  For example, using 
            skeptical semantics, an argument node would be labelled 'in, i.e. 
            assigned the value 1.0, if and only if it is in all extensions."}
  carneades.engine.argument-evaluation
  (:use carneades.engine.statement
        carneades.engine.argument-graph)
  (:gen-class))


(defprotocol ArgumentEvaluator
  "Protocol for argument evaluation structures."
  (evaluate [this ag] "evaluator argument-graph -> argument-graph")
  (label [this node] "evaluator node -> symbol"))

(defn standard-label? [key] (contains? #{:in, :out, :undecided} key))

(defn in-node? 
  "A statement or argument node is in iff its value is 1.0."
  [node]
  {:pre [(or (statement-node? node) (argument-node? node))]}
  (= (:value node) 1.0))

(defn out-node?
  "A statement or argument node is out iff its value is 0.0."
  [node]
  {:pre [(or (statement-node? node) (argument-node? node))]}
  (= (:value node) 0.0))

(defn undecided-node?
  "A statement or argument node is undecided iff it is neither in nor out."
  [node]
  {:pre [(or (statement-node? node) (argument-node? node))]}
  (not (or (in-node? node) (out-node? node))))

(defn node-standard-label
  "node -> symbol
   Returns the standard label of a statement or argument node."
  [node]
  {:pre [(or (statement-node? node) (argument-node? node))]}
  (cond (in-node? node) :in
        (out-node? node) :out
        :else :undecided))









