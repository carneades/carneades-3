;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns ^{:doc "Allows an external entity to be asked for information during a search. Experimental."}
    carneades.engine.ask
  ;(:require )
  (:use
    carneades.engine.unify
    carneades.engine.argument-search
    clojure.contrib.monads
    ;carneades.engine.argument
    )
  ;(:import )
  )

;(def to-engine (atom nil))
;(def from-engine (atom nil))

(defn ask-user
  [askable? to-engine-atom from-engine-atom]
  (fn [goal s]
    (let [g (apply-substitution (:substitutions s) goal)]
      (if (askable? g)
        ;(call-cc (fn [return] (get-answer g s return)))
        (let [new-from (promise),
              new-to (promise)]            
            (println "sending ask" g)
            (deliver @from-engine-atom (list 'ask g s new-to new-from)) ; send question
            ; update promises
            (reset! to-engine-atom new-to)
            (reset! from-engine-atom new-from)
            (println "ask-user waiting for answer")
            (let [answer @new-to ]; receive answer
                (println "answer received from user:" answer)
                answer)
            )
        nil))))

(defn reply
  ;[s goal answer log]
  [s goal answer]
  (let [subs1 (:substitutions s),
        subs2 (unify goal answer subs1)]
    ;(. log info (str "checking reply: " (print-str goal) " - " (print-str answer) " - " (print-str subs2)))
    ;(. log info (str "checking reply: " (print-str (map type goal)) " - " (print-str (map type answer)) " - " (print-str subs2)))
    ;(. log info (str "checking reply: " (print-str (type goal)) " - " (print-str (type answer)) " - " (print-str subs2)))
    (if subs2
      (do
        ;(. log info (str "constructing response from reply: " answer))
        (println "constructing response from reply:" answer)
        (list 
          (response
            subs2
            (carneades.engine.argument/argument
              (gensym 'a)
              :pro
              answer
              nil
              "claim"))))
      nil)))
