;;; Copyright (c) 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1


(ns ^{:doc "Built-in predicates for the rules: eval, =, not= etc."}
  carneades.engine.argument-builtins
  (:use carneades.engine.utils
        carneades.engine.statement
        carneades.engine.sandbox        
        carneades.engine.theory
        carneades.engine.utils
        carneades.engine.argument-generator
        carneades.engine.unify
        carneades.engine.argument))

(declare builtins)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; An argument generator for "builtin" predicates: eval, etc.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def builtin-theory
  (make-theory 
    :name "Carneades Builtin Theory"
    :sections
    (make-section
      :name "Priority"
      :schemes
      [(make-scheme 
        :name "Priority1"
        :conclusions ['(priority ?r2 ?r1 (not ?p1))]
        :premises ['(applies ?r2 ?p1)
                   '(prior ?r2 ?r1)])
      (make-scheme
        :name "Priority2"
        :conclusions ['(priority ?r2 ?r1 ?p1)]
        :premises ['(applies ?r2 (not ?p1)) 
                   '(prior ?r2 ?r1)])])))

(defn- dispatch-eval [subs literal term expr]
  (try
    (let [expr2 (apply-substitutions subs expr)]
      (if (not (ground? expr2))
        ()
        (let [result (eval-expr expr2)]
          (if-let [subs2 (unify term result subs)]
            (list (make-response subs2 () 
                                 (make-argument 
                                   :conclusion (literal->statement literal) 
                                   :scheme "builtin:eval")))
            ()))))
    (catch java.lang.SecurityException e ())
    (catch java.util.concurrent.TimeoutException e ())))

(defn- dispatch-equal [subs literal term1 term2]
  (if-let [subs2 (unify term1 term2 subs)]
    (list (make-response subs2 () 
                         (make-argument  
                           :conclusion (literal->statement literal) 
                           :scheme "builtin:=")))
    ()))

(defn- dispatch-notequal [subs literal term1 term2]
  (if-let [subs2 (unify term1 term2 subs)]
    ()
    (list (make-response subs () 
                         (make-argument  
                           :conclusion (literal->statement literal)
                           :scheme "builtin:not=")))))

(defn dispatch
  "literal substitutions (list-of generator) -> (stream-of response)"
  [literal subs generators]
    (if (and (compound-term? literal) (= (term-arity literal) 2))
      (let [[pre term1 term2] literal]
        (condp = pre
          'eval (dispatch-eval subs literal term1 term2)
          '= (dispatch-equal subs literal term1 term2)
          'not= (dispatch-notequal subs literal term1 term2)
          ; 'exists (dispatch-exists state literal wff generators)
          ; 'all (dispatch-all state literal wff generators)
          ()))
      ()))

(defn builtins
  "(seq-of generator) -> argument-generator"
  ([] (builtins ()))
  ([generators]
    (reify ArgumentGenerator
      (generate [this literal subs]
                (interleaveall
                  (generate (generate-arguments-from-theory builtin-theory) 
                            literal subs)
                  (dispatch literal subs generators))))))


