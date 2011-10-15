;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns ^{:doc "Allows an external entity to be asked for information during a search. Experimental."}
    carneades.engine.ask
  (:use
    carneades.engine.unify
    carneades.engine.response))

(defn ask-user
  [askable? get-answer]
  (fn [goal s]
    (let [g (apply-substitutions (:substitution s) goal)]
      (if (askable? g)
        (get-answer g s)
        nil))))

(defn reply
  [s goal answer]
  (let [stmt1 (:substitutions s),
        stmt2 (unify goal answer stmt1)]
    (if stmt2
      (response
        stmt2
        #{}
        (carneades.engine.argument/argument
          (gensym 'a)
          :pro
          answer
          nil
          "claim"))
      nil)))
