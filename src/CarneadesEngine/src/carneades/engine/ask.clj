;;; Copyright (c) 2010-2012 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns ^{:doc "Allows an external entity to be asked for information to construct arguments.  
            Experimental."}
  carneades.engine.ask
  (:use carneades.engine.unify
        carneades.engine.statement
        carneades.engine.argument
        carneades.engine.argument-generator))

(defn- pipe
 "Returns a pair: a seq (the read end) and a function (the write end).
  The function can takes either no arguments to close the pipe 
  or one argument which is appended to the seq. Read is blocking."
 ;; see http://clj-me.cgrand.net/2009/11/18/are-pipe-dreams-made-of-promises/
 []
 (let [promises (atom (repeatedly promise))
       p (second @promises)]
   [(lazy-seq @p)
    (fn 
      ([] ;close the pipe
         (let [[a] (swap! promises #(vector (second %)))]
           (if a
             (deliver a nil)
             (throw (Exception. "Pipe already closed")))))
      ([x] ;enqueue x
         (let [[a b] (swap! promises next)]
           (if (and a b)
             (do
               (deliver a (cons x (lazy-seq @b)))
               x)
             (throw (Exception. "Pipe already closed"))))))]))

(defn- generate-arguments-from-user
  [askable? user-answers send-question]
  (let [answers (atom user-answers)]
   (reify ArgumentGenerator
     (generate [this goal s]
       (let [g (apply-substitutions s goal)]
         (if (askable? g)
           (do
             (prn "SENDING ASK OF " g)
             (send-question [g s])
             (let [answers-value (deref answers)
                   _ (prn "WAITING FOR ANSWER")
                   answer (first answers-value)]
               (swap! answers rest)
               (println "ANSWER RECEIVED FROM USER:" answer)
               answer))
           (do
             (prn "not askable")
             nil))))
     ArgumentConstructionObserver
     (finish [this]
       ;; argument construction is finished? then
       ;; close pipe to inform the other end
       (send-question)))))

(defn make-argument-from-user-generator
  "Returns [generator seq-of-questions send-answer-fn]"
  [askable?]
  (let [answers-pipe (pipe)
        questions-pipe (pipe)
        generator (generate-arguments-from-user askable?
                                                (first answers-pipe)
                                                (second questions-pipe))]
    [generator (first questions-pipe) (second answers-pipe)]))

(defn make-answer
  "Creates an answer suitable to send to the argument-from-user generator"
  [s goal answer]
  (when-let [subs2 (unify (literal-atom goal) (literal-atom answer) s)]
    (println "goal = " goal)
    (println "constructing one answer from:" answer)
    (println "assumption = " answer)
    (list (make-response subs2 [answer]
                         nil
                         ;; (make-argument :conclusion answer :scheme "ask")
                         ))))


