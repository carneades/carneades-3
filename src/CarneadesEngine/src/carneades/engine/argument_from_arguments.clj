;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1


(ns carneades.engine.argument-from-arguments
  (:use clojure.contrib.pprint
        carneades.engine.utils
        carneades.engine.argument
        carneades.engine.unify
        [carneades.engine.argument-search :only (response)]))

;; generator for arguments from argument graphs
 
;; Essentially this generator interprets arguments as propositional implications
;; ("rules") 
;; and uses defeasible modus ponens to generate arguments
;; from these implications.


;; type generator: statement state  -> (seq-of response)
(defn generate-arguments-from-argument-graph [ag-as-kb]
  "generate-arguments: argument-graph -> generator"
  (fn [goal state]
    (let [subs (sget state :substitutions)
          proposition (apply-substitution subs goal)
          pro-args (pro-arguments ag-as-kb proposition)]
      (map (fn [arg]
             ;; no new substitutions, since propositional
             (response subs (assoc arg :id (gensym "a"))))
           pro-args))))
