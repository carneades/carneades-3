;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1


(ns ^{:doc "Generator for arguments from argument graphs.
            Essentially this generator interprets arguments as propositional implications (\"rules\") 
            and uses defeasible modus ponens to generate arguments
            from these implications."}
  carneades.engine.argument-from-arguments
  (:use clojure.contrib.pprint
        carneades.engine.utils
        carneades.engine.argument
        carneades.engine.unify
        [carneades.engine.argument-search :only (response)]))


;; type generator: statement state  -> (seq-of response)
(defn generate-arguments-from-argument-graph
  "generate-arguments: argument-graph -> generator

   Given an argument graph, return the following argument generator:

   Given a goal statement and a state, return a sequence of responses containing:

   1. The same substitution as the given state.
   2. A pro argument from the argument graph for the instantiated goal statement. "
  [ag-as-kb]
  (fn [goal state]
    (let [subs (sget state :substitutions)
          proposition (apply-substitution subs goal)
          pro-args (pro-arguments ag-as-kb proposition)]
      (map (fn [arg]
             ;; no new substitutions, since propositional
             (response subs #{} (assoc arg :id (gensym "a"))))
           pro-args))))
