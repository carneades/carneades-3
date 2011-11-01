;;; Copyright (c) 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1


(ns ^{:doc "Built-in predicates for the rules: eval, =, not= etc."}
  carneades.engine.argument-builtins
  (:use clojure.contrib.def
        clojure.contrib.pprint
        carneades.engine.utils
        carneades.engine.statement
        carneades.engine.sandbox        
        carneades.engine.rule
        carneades.engine.utils
        carneades.engine.argument-generator
        carneades.engine.unify
        carneades.engine.argument))

(declare builtins)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; An argument generator for "builtin" predicates: eval, etc.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar- *builtin-rules*
  (rulebase
   (rule* priority1   
          (if (and (applies ?r2 ?p1)
                   (prior ?r2 ?r1))
            (priority ?r2 ?r1 (not ?p1))))
   
   (rule* priority2
          (if (and (applies ?r2 (not ?p1)) 
                   (prior ?r2 ?r1))
            (priority ?r2 ?r1 ?p1)))))

(defn- dispatch-eval [subs stmt term expr]
  (try
    (let [expr2 (apply-substitutions subs (statement-wff expr))]
      (if (not (ground? expr2))
        ()
        (let [result (eval-expr expr2)]
          (if-let [subs2 (unify term result subs)]
            (list (make-response subs2 () 
                                 (make-argument 
                                   :conclusion stmt 
                                   :scheme "builtin:eval")))
            ()))))
    (catch java.lang.SecurityException e ())
    (catch java.util.concurrent.TimeoutException e ())))

(defn- dispatch-equal [subs stmt term1 term2]
  (if-let [subs2 (unify term1 term2 subs)]
    (list (make-response subs2 () 
                         (make-argument  
                           :conclusion stmt 
                           :scheme "builtin:=")))
    ()))

(defn- dispatch-notequal [subs stmt term1 term2]
  (if-let [subs2 (unify term1 term2 subs)]
    ()
    (list (make-response subs () 
                         (make-argument  
                           :conclusion stmt 
                           :scheme "builtin:not=")))))

(defn dispatch
  "stmt substitutions (list-of generator) -> (stream-of response)"
  [stmt subs generators]
  (let [wff (statement-wff stmt)]
    (if (seq? wff)
      (let [[pre term1 term2] wff]
        (condp = pre
          'eval (dispatch-eval subs stmt term1 term2)
          '= (dispatch-equal subs stmt term1 term2)
          'not= (dispatch-notequal subs stmt term1 term2)
          ; 'exists (dispatch-exists state stmt wff generators)
          ; 'all (dispatch-all state stmt wff generators)
          ()))
      ())))

(defn builtins
  "(seq-of generator) -> argument-generator"
  ([] (builtins ()))
  ([generators]
    (reify ArgumentGenerator
      (generate [this stmt subs]
                (interleaveall
                  (generate (generate-arguments-from-rules *builtin-rules*) 
                            stmt subs)
                  (dispatch stmt subs generators))))))


