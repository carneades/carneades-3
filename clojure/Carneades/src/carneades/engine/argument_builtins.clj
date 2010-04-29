(ns carneades.engine.argument-builtins
  (:use clojure.contrib.def
        clojure.contrib.pprint
        carneades.engine.utils
        carneades.engine.statement
        carneades.engine.argument
        [carneades.engine.argument-search :only (response)]
        carneades.engine.rule
        carneades.engine.unify))

(def *builtin-rules*
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
                     (list (struct response subs2 nil))
                     ;; fail:
                     '()))
                 (in-statements args (statement-predicate stmt))))

(defn dispatch [stmt state]
  "stmt state -> (stream-of response)"
  (let [args (:arguments state)
        subs (:substitutions state)
        wff (statement-wff stmt)
        [pre term1 term2 & lastterms] wff]
    (condp = pre
          'eval (throw (Exception. "NYI"))
          '= (throw (Exception. "NYI"))
          'not= (throw (Exception. "NYI"))
          (try-unify stmt args subs))))

(defn builtins [goal state]
  "statement state -> (stream-of response)"
  (interleaveall
   ((generate-arguments-from-rules *builtin-rules* []) goal state)
   (dispatch goal state)))
