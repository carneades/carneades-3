(ns impact.web.logic.question)

(defrecord Question
    [symbol
     arity
     forms
     category
     hint
     type
     followups])

(defn make-question
  [& kv]
  (merge (Question. (gensym "q")
                    1 ;; arity
                    {} ;; forms
                    {} ;; category
                    {} ;; hint
                    "text" ;; type
                    []) ;; followups
         (apply hash-map kv)))

(defn question? [x] (instance? Question x))