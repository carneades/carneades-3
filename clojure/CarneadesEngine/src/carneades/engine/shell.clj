;;; Carneades Argumentation Library and Tools.
;;; Copyright (C) 2010 Thomas F. Gordon, Fraunhofer FOKUS, Berlin
;;; 
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU Lesser General Public License version 3 (LGPL-3)
;;; as published by the Free Software Foundation.
;;; 
;;; This program is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for details.
;;; 
;;; You should have received a copy of the GNU Lesser General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

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

;; (defn show-state [state]
;;   "view a diagram of the argument graph of a state"
;;   (view (sget state :arguments)))

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
