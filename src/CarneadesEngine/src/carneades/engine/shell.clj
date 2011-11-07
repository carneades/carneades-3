;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns ^{:doc "Utilities for interacting with the Carneades engine from a command line."}
  carneades.engine.shell
  (:use carneades.engine.statement
        carneades.engine.unify
        carneades.engine.argument-graph
        carneades.engine.argument-construction
        carneades.engine.argument-evaluation
        carneades.engine.case))
  
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
  "engine evaluator literal -> (seq-of literal)" 
  ([engine query]
    (ask engine carneades-evaluator query))
  ([engine evaluator query] 
   {:pre [(literal? query)]}
    (filter (fn [sn] 
              (let [subs (unify (:atom sn) (literal-atom query))]
                (if (not subs) 
                    ()
                    (if (or (and (literal-pos? query) (in? sn)) 
                            (and (literal-neg? query) (out? sn)))
                        (apply-substitutions subs query)))))
            (:statement-nodes (evaluate evaluator (engine query))))))

(defn succeed?
  "engine evaluator statement (set-of literal) -> boolean
   returns true iff the set returned by the engine equals the solution set"
  ([engine query solutions]
    (succeed? engine carneades-evaluator solutions))
  ([engine evaluator query solutions]
    (= (set (ask engine evaluator query)) solutions)))
  
(defn fail?
  ([engine query solutions]
    (fail? engine carneades-evaluator solutions))
  ([engine evaluator query solutions]
    (not (succeed? engine evaluator query solutions))))

