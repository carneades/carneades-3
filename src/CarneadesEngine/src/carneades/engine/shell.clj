;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1


(ns carneades.engine.shell
  (:use clojure.contrib.pprint
        carneades.engine.utils
        carneades.engine.argument-search
        [carneades.engine.search :only (depth-first resource)]
        carneades.ui.diagram.viewer)
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

(defn unite-solutions
  [sols]
  (arg/unite-argument-graphs (map :arguments sols)))

(defn add-candidates
  [ag candidates subs]
  (arg/assert-arguments ag (map
                         (fn [c]
                           (arg/instantiate-argument
                             (:argument c)
                             subs))
                         candidates)))

(defn unite-solutions-with-candidates
  [sols]
  (arg/unite-argument-graphs
    (map (fn [s] (add-candidates
                   (:arguments s)
                   (:candidates s)
                   (:substitutions s)))
      sols)))

(defn construct-arguments [goal max-nodes max-turns ag generators]
  "integer integer argument-graph (seq-of generator) -> statement ->
(seq-of state)"  
  (construct-best-arguments depth-first max-nodes max-turns
    (initial-state goal ag) generators))

(defn make-engine* [max-nodes max-turns ag generators]
  "integer integer argument-graph (seq-of generator) -> statement -> 
   (seq-of state)"
  (fn [goal]
    (find-best-arguments depth-first max-nodes max-turns
                         (initial-state goal ag) generators)))

(defn make-engine [max-nodes max-turns generators]
  "integer integer (seq-of generator) -> statement -> (seq-of state)
 
   a simplified version of make-engine*, using the default-context "
  (make-engine* max-nodes max-turns arg/*empty-argument-graph* generators))

(defn ask
  " ask: statement (statement -> (stream-of state)) -> void

    Displays the query with the substitions found in each state
    produced by the given inference engine or prints nothing if the stream is emtpy.
    Always terminates, as only states found given the resource limit of the
    inference engine will be displayed."
  [query engine]
  (map (fn [s] (pprint ((:substitutions s) query))) (solutions (engine query))))

; (defn show-state [state]
;   "view a diagram of the argument graph of a state"
;  (view (sget state :arguments)))

;; (defn show
;;   ([query engine]
;;      (show query engine true))
;;   ([query engine showall]
;;      (let [states (engine query)]
;;        (if showall
;;          (doseq [s states]
;;            (show-state s))
;;          (when-not (empty? states)
;;            (show-state (first states)))))))

;; (defn show1 [query engine]
;;   (show query engine false))
