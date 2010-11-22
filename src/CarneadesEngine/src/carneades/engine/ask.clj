
(ns carneades.engine.ask
  ;(:require )
  (:use
    carneades.engine.unify
    carneades.engine.argument-search
    )
  ;(:import )
  )

(defn ask-user
  [askable? get-answer]
  (fn [goal state]
    (let [g (apply-substitution (:substitution state) goal)]
      (if (askable? g)
        (get-answer g state)
        nil))))

(defn reply
  [state goal answer]
  (let [subs1 (:substitutions state),
        subs2 (unify goal answer subs1)]
    (if subs2
      (make-response
        subs2
        (argument
          (gensym 'a)
          'pro
          answer
          nil
          "claim"))
      nil)))
