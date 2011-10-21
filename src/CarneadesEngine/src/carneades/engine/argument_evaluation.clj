(ns ^{:doc "The protocol for argument evaluation structures."}
  carneades.engine.argument-evaluation
  (:use carneades.engine.statement
        carneades.engine.argument-structure))

;; Following Henry Prakken, an argument is:
;; - justified, if it is in all extensions 
;; - overruled, if it is in no extensions
;; - defensible, if it is some extension.

;; Similarly, a statement is justified, overruled or defensible iff it is the conclusion
;; of a justified, overruled or defensible argument, respectively.

;; Argument Evaluators can have different semantics, by defining extensions in different ways.
;; One way to define an argument evaluator is by defining a mapping from argument graphs
;; to Dung Argument Frameworks and then selecting a particular semantics for Dung AFs, such
;; as grounded or preferred semantics.

(defprotocol ArgumentEvaluator
  "Protocol for argument evaluation structures."
  (justified-statements [ag] 
    "Returns the set of statements in the argument graph which are in all extensions. ")   
  (overruled-statements [ag]
    "Returns the set of statements in the argument graph which are in no extensions.")
  (defensible-statements [ag]   
    "Returns the set of statements in the argument graph which are in some extension."))

