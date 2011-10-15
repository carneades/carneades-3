;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns ^{:doc "Utilities for interacting with the Carneades engine from a command line."}
  carneades.engine.shell
  (:use carneades.engine.argument
        carneades.engine.argument-construction))
  
(defn make-engine
  "argument-graph integer (seq-of statement) (seq-of generator) -> 
   statement -> argument-graph)"
  ([max-goals assumptions generators]
    (make-engine (argument-graph) max-goals assumptions generators))
  ([argument-graph max-goals assumptions generators]
    (fn [issue]
      (construct-arguments argument-graph issue max-goals assumptions generators))))
  
(defn argue
  "engine statement -> argument-graph"
  [engine issue]
  (engine issue))

(defn ask 
  "engine statement -> (seq-of statement)" 
  [engine query] 
  (matching-in-statements (engine query) query))

(defn succeed?
  "engine statement (set-of statement) -> boolean
   returns true iff the set returned by the engine equals the solution set"
  [engine query solutions]
  (= (set (ask engine query)) solutions))
  
(defn fail?
  [engine query solutions]
  (not (succeed? engine query solutions)))