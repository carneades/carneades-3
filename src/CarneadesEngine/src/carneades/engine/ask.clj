;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns ^{:doc "Allows an external entity to be asked for information during a search. Experimental."}
    carneades.engine.ask
  ;(:require )
  (:use
    carneades.engine.unify
    carneades.engine.argument-search
    ;carneades.engine.argument
    )
  ;(:import )
  )

(defn ask-user
  [askable? get-answer]
  (fn [goal s]
    (let [g (apply-substitution (:substitution s) goal)]
      (if (askable? g)
        (get-answer g s)
        nil))))

(defn reply
  [s goal answer]
  (let [subs1 (:substitutions s),
        subs2 (unify goal answer subs1)]
    (if subs2
      (response
        subs2
        #{}
        (carneades.engine.argument/argument
          (gensym 'a)
          'pro
          answer
          nil
          "claim"))
      nil)))
