;;; Copyright (c) 2010-2011 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1


(ns ^{:doc "Built-in predicates for the rules: eval, =, not= etc."}
  carneades.engine.argument-builtins
  (:use clojure.pprint
        carneades.engine.utils
        carneades.engine.statement
        carneades.engine.sandbox        
        carneades.engine.scheme
        carneades.engine.utils
        carneades.engine.argument-generator
        carneades.engine.unify
        carneades.engine.dublin-core
        carneades.engine.argument))

(declare builtins)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; An argument generator for "builtin" predicates: eval, etc.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def builtin-theory
  (make-theory 
    :header 
    (make-metadata :title "Carneades Builtin Theory")
    :schemes
    [(make-scheme 
       :id 'priority1
       :conclusion '(priority ?r2 ?r1 (not ?p1))
       :premises [(pm '(applies ?r2 ?p1))
                  (pm '(prior ?r2 ?r1))])
     (make-scheme
       :id 'priority2
       :conclusion '(priority ?r2 ?r1 ?p1)
       :premises [(pm '(applies ?r2 (not ?p1))) 
                  (pm '(prior ?r2 ?r1))])]))

(defn- dispatch-eval [subs literal term expr]
  (let [expr2 (apply-substitutions subs expr)]
    (if (not (ground? expr2))
      ()
      (let [result (eval-expr expr2)]
        (if-let [subs2 (unify term result subs)]
          (list (make-response subs2 () 
                               (make-argument 
                                :conclusion literal 
                                :scheme `(~'builtin:eval ,result ,expr2))))
          ())))))

(defn- dispatch-equal [subs literal term1 term2]
  (if-let [subs2 (unify term1 term2 subs)]
    (list (make-response subs2 () 
                         (make-argument  
                           :conclusion literal 
                           :scheme `(~'builtin:= ,term1 ,term2))))
    ()))

(defn- dispatch-notequal [subs literal term1 term2]
  (if-let [subs2 (unify term1 term2 subs)]
    ;; (do (println "subs2:" subs2) ())
    (list (make-response subs () 
                         (make-argument  
                           :conclusion literal
                           :scheme `(~'builtin:not= ,term1 ,term2))))))

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
                (dispatch literal subs generators)))))

;; The Builtin theory is commented out for now, since it currently 
;; only includes schemes for reasoning about rule priorities, and 
;; this feature is hardly used and may need to be redesigned.
;                (interleaveall
;                  (generate (generate-arguments-from-theory builtin-theory) 
;                            literal subs)
;                  (dispatch literal subs generators))))))


