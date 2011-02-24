
(ns carneades.engine.ask
  ;(:require )
  (:use
    carneades.engine.unify
    carneades.engine.argument-search
    clojure.contrib.monads
    ;carneades.engine.argument
    )
  ;(:import )
  )

(defn ask-user
  [askable? get-answer]
  (fn [goal s]
    (let [g (apply-substitution (:substitution s) goal)]
      (if (askable? g)
        ;(call-cc (fn [return] (get-answer g s return)))
        (get-answer g s)
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
