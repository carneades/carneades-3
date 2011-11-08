;;; Copyright (c) 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns ^{:doc "Utilities for interacting with the Carneades engine from a command line."}
  carneades.engine.shell
  (:use carneades.engine.statement
        carneades.engine.unify
        carneades.engine.argument-graph
        carneades.engine.argument-construction
        carneades.engine.argument-evaluation
        carneades.engine.caes))
  
(defn make-engine
  "argument-graph integer (seq-of literal) (seq-of generator) -> 
   literal -> argument-graph)"
  ([max-goals assumptions generators]
    (make-engine (make-argument-graph) max-goals assumptions generators))
  ([argument-graph max-goals assumptions generators]
    (fn [issue]
      (construct-arguments argument-graph issue max-goals assumptions generators))))
  
(defn argue
  "engine literal  -> argument-graph"
  [engine issue]
  {:pre [(literal? issue)]}
  (engine issue))  

(defn ask 
  "engine evaluator literal -> (seq-of literal)" 
  ([engine query]
    (ask engine carneades-evaluator query))
  ([engine evaluator query] 
   {:pre [(literal? query)]}
    (mapcat (fn [sn] 
              (let [subs (unify (:atom sn) (literal-atom query))]
                (if (not subs) 
                    ()
                    (if (or (and (literal-pos? query) (in? sn)) 
                            (and (literal-neg? query) (out? sn)))
                        (list (apply-substitutions subs query))))))
            (vals (:statement-nodes (evaluate evaluator (argue engine query)))))))

(defn succeed?
  "engine evaluator literal literal -> boolean
   returns true iff the given literal is a member of the set returned by the engine"
  ([engine query literal]
    (succeed? engine carneades-evaluator query literal))
  ([engine evaluator query literal]
    (contains? (set (ask engine evaluator query)) literal)))
  
(defn fail?
   "engine evaluator literal literal -> boolean
   returns true iff the given literal is not a member of the set returned by the engine"
  ([engine query literal]
    (fail? engine carneades-evaluator query literal))
  ([engine evaluator query literal]
    (not (succeed? engine evaluator query literal))))

