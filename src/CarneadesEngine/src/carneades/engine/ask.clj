;;; Copyright (c) 2010-2012 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns ^{:doc "Allows an external entity to be asked for information to construct arguments.  
            Experimental."}
  carneades.engine.ask
  (:use carneades.engine.unify
        carneades.engine.argument
        carneades.engine.argument-generator))

(defn ask-user
  [askable? to-engine-atom from-engine-atom]
  (prn "creating ask-user generator")
  (reify ArgumentGenerator
    (generate [this goal s]
      (prn "in ask-user")
      (let [g (apply-substitutions (:substitutions s) goal)]
        (if (askable? g)
          (let [new-from (promise)
                new-to (promise)]            
            (println "sending ask" g)
            (deliver @from-engine-atom (list 'ask g s new-to new-from)) ; send question
                                        ; update promises
            (reset! to-engine-atom new-to)
            (reset! from-engine-atom new-from)
            (println "ask-user waiting for answer")
            (let [answer @new-to ]      ; receive answer
              (println "answer received from user:" answer)
              answer))
          nil)))))

;; (defn reply
;;   [answer]
;;   (prn "reply...")
;;   (reify ArgumentGenerator
;;     (generate [this literal sub]
;;       (let [subs2 (unify literal answer subs)]
;;         (if subs2
;;           (list (make-response 
;;                  subs2
;;                  ()
;;                  (make-argument
;;                   :conclusion answer
;;                   :scheme "ask")))
;;           ())))))


(defn reply
  [s goal answer]
  (let [subs1 (:substitutions s),
        subs2 (unify goal answer subs1)]
    (if subs2
      (do
        (println "constructing response from reply:" answer)
        (list (make-response 
               subs2
               ()
               (make-argument
                :conclusion answer
                :scheme "ask"))))
      nil)))