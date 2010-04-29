(ns carneades.engine.shell
  (:use clojure.contrib.pprint
        carneades.engine.argument-search
        [carneades.engine.search :only (depth-first resource)])
  (:require [carneades.engine.argument :as arg]))

(defn solutions [states]
  "(seq-of state) -> (seq-of state)

  A state is a \"solution\" if the instantiated topic of the state is in."
  (filter (fn [s]
            (let [sub ((:substitutions s) (:topic s))]
              (arg/in? (:arguments s) sub)))
          states))

(defn succeed? [query engine]
  "engine -> boolean

    True if at least one goal state was found by the engine
  "
  (not (empty? (solutions (engine query)))))

(defn fail? [query engine]
  "engine -> boolean

    True if no state found by the engine is a goal state
  "
  (empty? (solutions (engine query))))

(defn make-engine* [max-nodes max-turns ag generators]
  "integer integer argument-graph (seq-of generator) -> statement -> 
   (seq-of state)"
  (fn [goal]
    (find-best-arguments depth-first max-turns
                         (initial-state goal ag) generators)))

(defn make-engine [max-nodes max-turns generators]
  "integer integer (seq-of generator) -> statement -> (seq-of state)
 
   a simplified version of make-engine*, using the default-context "
  (make-engine* max-nodes max-turns arg/*empty-argument-graph* generators))

