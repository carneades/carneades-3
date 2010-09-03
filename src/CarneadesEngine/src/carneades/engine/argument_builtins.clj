;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1


(ns carneades.engine.argument-builtins
  (:use clojure.contrib.def
        clojure.contrib.pprint
        carneades.engine.utils
        carneades.engine.statement
        carneades.engine.argument
        carneades.engine.sandbox        
        carneades.engine.rule
        carneades.engine.utils
        ;carneades.engine.shell
        [carneades.engine.search :only (breadth-first)]
        carneades.engine.unify)
  (:require
    [carneades.engine.argument-search :as as]
    ))
    clojure.contrib.pprint
    carneades.engine.utils
    carneades.engine.statement
    carneades.engine.argument
    carneades.engine.sandbox
    carneades.engine.rule
    carneades.engine.utils
    ;carneades.engine.shell
    [carneades.engine.search :only (breadth-first search)]
    carneades.engine.unify)
  (:require
    [carneades.engine.argument-search :as as]
    ))

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
                     (list (as/response subs2 nil))

                     ;; fail:
                     '()))
                 (in-statements args (statement-predicate stmt))))
    (in-statements args (statement-predicate stmt))))

(defn- dispatch-eval [subs stmt term expr]
  (try
    (let [result (eval-expr (subs (statement-wff expr)))]
      (if-let [subs2 (unify term result subs)]
        (list (as/response subs2 (argument (gensym "a") :pro stmt '()

                                        "builtin:eval")))
                                   "builtin:eval")))
        '()))
    (catch java.lang.SecurityException e '())
    (catch java.util.concurrent.TimeoutException e '())))

(defn- dispatch-equal [subs stmt term1 term2]
  (if-let [subs2 (unify term1 term2 subs)]
    (list (as/response subs2 (argument (gensym "a") :pro stmt '()

                                    "builtin:=")))
                               "builtin:=")))
    '()))

(defn- dispatch-notequal [subs stmt term1 term2]
  (if-let [subs2 (unify term1 term2 subs)]
    '()
    (list (as/response subs (argument (gensym "a") :pro stmt '()

                                    "builtin:not=")))))
                              "builtin:not=")))))

(defn- dispatch-exists
  [state stmt wff generators]
  (let [subs (:substitutions state),
        [e v t p] (subs wff),
        v2 (gensym "?"),
        t2 (replace-var v v2 t),
        p2 (replace-var v v2 p),
        type-states (as/find-best-arguments
                      breadth-first
                      nil
                      0
                      (as/state
                        t2
                        :pro 
                        (list (list t2)) 
                        '()
                        (:arguments state)
                        (:substitutions state)
                        (:candidates state))
                        (lazy-cat generators (lazy-seq (list (builtins generators))))),
        ] 
;    (println "exists dispatch:" (count type-states) "found")
;    (println "generators" generators)
;    (println "  v :" v)
;    (println "  v2:" v2)
;    (println "  t :" t)
;    (println "  t2:" t2)
;    (println "  p :" p)
;    (println "  p2:" p2)
    (map (fn [s]
           (let [new-subs (:substitutions s)]
             (as/response
               new-subs
               (cons
                 (argument
                   (gensym "exists")
                   :pro 
                   stmt
                   (list
                     (pm (new-subs p2))
                     (am (new-subs t2)))
                   "exists")
                 (vals (:arguments (:arguments s)))))))
      type-states)))

(defn dispatch [stmt state generators]
  "stmt state (list-of generator) -> (stream-of response)"
(defn- dispatch-exists
  [state stmt wff generators]
  (let [subs (:substitutions state),
        [e v t p] (subs wff),
        v2 (gensym "?"),
        t2 (replace-var v v2 t),
        p2 (replace-var v v2 p),
        type-states (as/find-best-arguments
                      search
                      breadth-first
                      nil
                      0
                      (as/state
                        t2
                        :pro 
                        (list (list t2)) 
                        '()
                        (:arguments state)
                        (:substitutions state)
                        (:candidates state))
                      (lazy-cat generators (lazy-seq (list (builtins generators))))),
        ] 
    ;    (println "exists dispatch:" (count type-states) "found")
    ;    (println "generators" generators)
    ;    (println "  v :" v)
    ;    (println "  v2:" v2)
    ;    (println "  t :" t)
    ;    (println "  t2:" t2)
    ;    (println "  p :" p)
    ;    (println "  p2:" p2)
    (map (fn [s]
           (let [new-subs (:substitutions s)]
             (as/response
               new-subs
               (cons
                 (argument
                   (gensym "exists")
                   :pro 
                   stmt
                   (list
                     (pm (new-subs p2))
                     (am (new-subs t2)))
                   "exists")
                 (arguments (:arguments s))))))
      type-states)))

(defn state->premises
  [s t p]
  (let [subs (:substitutions s)]
    (list
      (pm (subs p))
      (am (subs t)))))

(defn- dispatch-all
  [state stmt wff generators]
  (println "dispatch-all" wff)
  (let [subs (:substitutions state),
        [e v t p] (subs wff),
        v2 (gensym "?"),
        t2 (replace-var v v2 t),
        p2 (replace-var v v2 p),
        type-states (as/find-best-arguments
                      search
                      breadth-first
                      nil
                      0
                      (as/state
                        t2
                        :pro
                        (list (list t2))
                        '()
                        (:arguments state)
                        (:substitutions state)
                        (:candidates state))
                      (lazy-cat generators (lazy-seq (list (builtins generators))))),
        premises (apply concat (map (fn [s] (state->premises s t2 p2)) type-states)),
        arg (argument
              (gensym "all")
              :pro
              stmt
              premises
              "all")
        ]
    (println "all dispatch:" (count type-states) "found")
    (println "generators" generators)
    (println "  v :" v)
    (println "  v2:" v2)
    (println "  t :" t)
    (println "  t2:" t2)
    (println "  p :" p)
    (println "  p2:" p2)
    (println "  a :" arg)
    (list
      (as/response
        subs
        (cons arg
            (apply concat (map arguments (map :arguments type-states))))))))

(defn dispatch [stmt state generators]
  "stmt state (list-of generator) -> (stream-of response)"
  (let [args (:arguments state)
        subs (:substitutions state)
        wff (statement-wff stmt)
        [pre term1 term2] wff]
    (condp = pre
  (let [wff (statement-wff stmt)]
    (if (seq? wff)
      (let [args (:arguments state)
            subs (:substitutions state)
            [pre term1 term2] wff]
        (condp = pre
          'eval (dispatch-eval subs stmt term1 term2)
          '= (dispatch-equal subs stmt term1 term2)
          'not= (dispatch-notequal subs stmt term1 term2)
          'exists (dispatch-exists state stmt wff generators)
          (try-unify stmt args subs))))
          'all (dispatch-all state stmt wff generators)
          (try-unify stmt args subs)))
      nil)))

(defn builtins  
  ([goal state]
    "statement state -> (stream-of response)"
    (interleaveall
      ((generate-arguments-from-rules *builtin-rules* []) goal state)
      (dispatch goal state '())))
  ([generators]
    "(list-of generator) -> (statement state -> (seq-of response))"
    (fn [goal state]
      (interleaveall
        ((generate-arguments-from-rules *builtin-rules* []) goal state)
        (dispatch goal state generators)))))





