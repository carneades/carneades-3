;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1


(ns carneades.engine.argument-builtins
  (:use clojure.contrib.def
        clojure.contrib.pprint
        carneades.engine.utils
        carneades.engine.statement
        carneades.engine.argument
        carneades.engine.sandbox
        [carneades.engine.argument-search :only (response)]
        carneades.engine.rule
        carneades.engine.unify))

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

(defn- try-unify [stmt args subs]
  (mapinterleave (fn [stmt2]
                   (if-let [subs2 (unify stmt stmt2 subs)]
                     (list (response subs2 nil))
                     ;; fail:
                     '()))
                 (in-statements args (statement-predicate stmt))))

(defn- dispatch-eval [subs stmt term expr]
  (try
    (let [result (eval-expr (subs (statement-wff expr)))]
      (if-let [subs2 (unify term result subs)]
        (list (response subs2 (argument (gensym "a") :pro stmt '()
                                        "builtin:eval")))
        '()))
    (catch java.lang.SecurityException e '())
    (catch java.util.concurrent.TimeoutException e '())))

(defn- dispatch-equal [subs stmt term1 term2]
  (if-let [subs2 (unify term1 term2 subs)]
    (list (response subs2 (argument (gensym "a") :pro stmt '()
                                    "builtin:=")))
    '()))

(defn- dispatch-notequal [subs stmt term1 term2]
  (if-let [subs2 (unify term1 term2 subs)]
    '()
    (list (response subs (argument (gensym "a") :pro stmt '()
                                    "builtin:not=")))))

(defn dispatch [stmt state]
  "stmt state -> (stream-of response)"
  (let [args (:arguments state)
        subs (:substitutions state)
        wff (statement-wff stmt)
        [pre term1 term2] wff]
    (condp = pre
          'eval (dispatch-eval subs stmt term1 term2)
          '= (dispatch-equal subs stmt term1 term2)
          'not= (dispatch-notequal subs stmt term1 term2)
          (try-unify stmt args subs))))

(defn builtins [goal state]
  "statement state -> (stream-of response)"
  (interleaveall
   ((generate-arguments-from-rules *builtin-rules* []) goal state)
   (dispatch goal state)))
