;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1


(ns ^{:doc "Built-in predicates for the rules: eval, =, not= etc."}
  carneades.engine.argument-builtins
  (:use clojure.contrib.def
        clojure.contrib.pprint
        carneades.engine.utils
        carneades.engine.statement
        carneades.engine.argument
        carneades.engine.sandbox        
        carneades.engine.rule
        carneades.engine.utils
        ;carneades.engine.shell
        [carneades.engine.search :only (breadth-first search)]
        carneades.engine.unify
        carneades.engine.response))

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
    (let [result (eval-expr (subs (statement-wff expr)))]
      (if-let [subs2 (unify term result subs)]
        (list (make-response subs2 #{} (argument (gensym "a") :pro stmt '() "builtin:eval")))
        '()))
    (catch java.lang.SecurityException e '())
    (catch java.util.concurrent.TimeoutException e '())))

(defn- dispatch-equal [subs stmt term1 term2]
  (if-let [subs2 (unify term1 term2 subs)]
    (list (make-response subs2 #{} (argument (gensym "a") :pro stmt '() "builtin:=")))
    '()))

(defn- dispatch-notequal [subs stmt term1 term2]
  (if-let [subs2 (unify term1 term2 subs)]
    '()
    (list (make-response subs #{} (argument (gensym "a") :pro stmt '() "builtin:not=")))))

;(defn- dispatch-exists
;  [state stmt wff generators]
;  (let [subs (:substitutions state),
;        [e v t p] (apply-substitution subs wff),
;        v2 (gensym "?"),
;        t2 (replace-var v v2 t),
;        p2 (replace-var v v2 p),
;        type-states (as/find-best-arguments
;                      search
;                      breadth-first
;                      nil
;                      0
;                      (as/state
;                        t2
;                        :pro 
;                        (list (list t2)) 
;                        '()
;                        (:arguments state)
;                        (:substitutions state)
;                        (:candidates state))
;                      (lazy-cat generators (lazy-seq (list (builtins generators))))),
;        ] 
;    ;    (println "exists dispatch:" (count type-states) "found")
;    ;    (println "generators" generators)
;    ;    (println "  v :" v)
;    ;    (println "  v2:" v2)
;    ;    (println "  t :" t)
;    ;    (println "  t2:" t2)
;    ;    (println "  p :" p)
;    ;    (println "  p2:" p2)
;    (map (fn [s]
;           (let [new-subs (:substitutions s)]
;             (make-response
;               new-subs
;               #{}
;               (cons
;                 (argument
;                   (gensym "exists")
;                   :pro 
;                   stmt
;                   (list
;                     (pm (apply-substitution new-subs p2))
;                     (pm (apply-substitution new-subs t2)))
;                   "exists")
;                 (arguments (:arguments s))))))
;      type-states)))

(defn state->premises
  [s t p]
  (let [subs (:substitutions s)]
    (list
      (pm (apply-substitution subs p))
      (pm (apply-substitution subs t)))))

;(defn- dispatch-all
;  [state stmt wff generators]
;  (println "dispatch-all" wff)
;  (let [subs (:substitutions state),
;        [e v t p] (apply-substitution subs wff),
;        v2 (gensym "?"),
;        t2 (replace-var v v2 t),
;        p2 (replace-var v v2 p),
;        type-states (as/find-best-arguments
;                      search
;                      breadth-first
;                      nil
;                      0
;                      (as/state
;                        t2
;                        :pro
;                        (list (list t2))
;                        '()
;                        (:arguments state)
;                        (:substitutions state)
;                        (:candidates state))
;                      (lazy-cat generators (lazy-seq (list (builtins generators))))),
;        premises (apply concat (map (fn [s] (state->premises s t2 p2)) type-states)),
;        arg (argument
;              (gensym "all")
;              :pro
;              stmt
;              premises
;              "all")
;        ]
;    (println "all dispatch:" (count type-states) "found")
;    (println "generators" generators)
;    (println "  v :" v)
;    (println "  v2:" v2)
;    (println "  t :" t)
;    (println "  t2:" t2)
;    (println "  p :" p)
;    (println "  p2:" p2)
;    (println "  a :" arg)
;    (list
;      (make-response
;        subs
;        #{}
;        (cons arg
;            (apply concat (map arguments (map :arguments type-states))))))))

(defn dispatch
  "stmt substitutiosn (list-of generator) -> (stream-of response)"
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
          nil))
      nil)))

; type generator : statement substitutions -> (seq-of response)

(defn builtins
  "(list-of generator) -> generator"
  ([] (builtins '()))
  ([generators]
    (fn [goal subs]
      (interleaveall
        ((generate-arguments-from-rules *builtin-rules*) goal subs)
        (dispatch goal subs generators)))))
