;;; Copyright (c) 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns ^{:doc "Allows an external entity to be asked for information to construct arguments.  
            Experimental."}
    carneades.engine.ask
  (:use
    carneades.engine.unify
    carneades.engine.argument
    carneades.engine.argument-generator))

(defn ask
  [askable? get-answer]
  (fn [goal s]
    (let [g (apply-substitutions (:substitution s) goal)]
      (if (askable? g)
        (get-answer g s)
        ()))))

(defn reply
  [s goal answer]
  (let [subs (unify goal answer (:substitutions s))]
    (if subs
      (make-response 
        stmt2
        #{}
        (make-argument
          :conclusion answer
          :scheme "claim"))
      ())))

(defn repl
  [answer]
  (reify ArgumentGenerator
    (generate [stmt sub]
              (let [subs2 (unify stmt answer subs)]
                (if subs2
                  (list (make-response 
                          stmt2
                          #{}
                          (make-argument
                            :conclusion answer
                            :scheme "ask")))
                  ())))))