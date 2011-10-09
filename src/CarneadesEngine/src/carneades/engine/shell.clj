;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns ^{:doc "Utilities for interacting with the Carneades engine from a command line."}
    carneades.engine.shell
  (:use clojure.set
        clojure.contrib.pprint
        clojure.contrib.profile
        carneades.engine.utils
        [carneades.engine.abduction :as abd]
        carneades.engine.argument-search
        carneades.engine.unify
        [carneades.engine.argument :only (node-statement get-nodes arguments)]
        [carneades.engine.search :only (depth-first resource search traverse)])
  (:require [carneades.engine.argument :as arg]))

 (defn make-engine
   "integer integer argument-graph (seq-of generator) -> statement -> (seq-of state)"
   ([max-nodes max-turns generators]
     (make-engine max-nodes max-turns arg/*empty-argument-graph* generators))
   ([max-nodes max-turns argument-graph generators]
     (fn [goal]
       (find-best-arguments depth-first max-nodes max-turns
                            (initial-state goal argument-graph) generators))))

(defn solutions
  "(seq-of state) -> (seq-of state)
  A state is a 'solution' if the instantiated topic of the state is in."
  [states]
  (filter (fn [s]
            (let [sub (apply-substitution (:substitutions s) (:topic s))]
              (arg/in? (:arguments s) sub)))
          states))

(defn succeed?
  "engine -> boolean
   True if at least one goal state was found by the engine"
  [query engine]
  (not (empty? (solutions (engine query)))))

(defn fail?
  "engine -> boolean
   True if no state found by the engine is a goal state"
  [query engine]
  (empty? (solutions (engine query))))

(defn ask
  " ask: statement (statement -> (stream-of state)) -> (seq-of statement)
    Returns the sequence of answers to a query found using the given inference engine.
    Always terminates, as only answers found given the resource limit of the
    inference engine will be displayed."
  [engine query]
  (map (fn [s] (apply-substitution (:substitutions s) query)) 
       (solutions (engine query))))
  
(defn argue
  "statement int int (set-of statement) (seq-of generator) -> argument-graph
   Construct an argument graph for both sides of an issue."
  [issue max-nodes max-turns assumptions generators]
  (let [ag (-> (assoc (arg/argument-graph) :main-issue issue) 
               (arg/accept assumptions))]
       (:arguments (construct-arguments (initial-state issue ag) 
                                        max-nodes max-turns generators)))) 


;;;;;;  Is the stuff below still needed or used anywhere?

;(defn- search-graph [pred objects]
;  (let [n-ahead 100]
;   (seque n-ahead (keep (fn [obj]
;                          (when (pred obj)
;                            obj))
;                        objects))))
;
;(defn- stmt-pred [stmt stmt-fmt to-search]
;  (let [formatted (stmt-fmt stmt)]
;    (.contains (.toLowerCase formatted) to-search)))
;
;(defn- arg-pred [arg to-search]
;  (let [title (:title arg)]
;    (if-not (nil? title)
;      (.contains (.toLowerCase title) to-search)
;      false)))
;
;(defn search-statements
;  "Produces a sequence of statements satisfying the search options.
;   The sequence is produced in the background with seque. The 
;   reading from the sequence can block if the reader gets ahead of the
;   search
;
;   The keys for options are ..."
;  [ag stmt-fmt search-content options]
;  (let [to-search (.toLowerCase search-content)
;        stmts (map node-statement (get-nodes ag))]
;    (search-graph #(stmt-pred % stmt-fmt to-search) stmts)))
;
;(defn search-arguments
;  "See search-statements"
;  [ag search-content options]
;  (let [to-search (.toLowerCase search-content)]
;    (search-graph #(arg-pred % to-search) (arguments ag))))
;
;(defn search-all
;  "Returns a seq of the form ((:stmt stmt1) (:arg arg1) (:stmt stmt2) ...)"
;  [ag stmt-fmt search-content options]
;  (let [to-search (.toLowerCase search-content)
;        stmts (search-statements ag stmt-fmt search-content options)
;        args (search-arguments ag search-content options)]
;    (interleaveall (partition 2 (interleave (repeat :stmt) stmts))
;                   (partition 2 (interleave (repeat :arg) args)))))
;
;