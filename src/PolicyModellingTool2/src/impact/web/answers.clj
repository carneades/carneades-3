(ns impact.web.answers
  (:use clojure.pprint
        carneades.engine.statement
        [carneades.engine.ask :only (reply)]))

(defn add-answers
  [service-data answers]
  {:pre [(not (nil? service-data))]}
  (prn "add-answers")
  (prn answers)
  (let [existing-answers (:answers service-data)
        ;; last-question (:last-question service-data)
        ;; adds answers and index them on pred or [pred sub]
        existing-answers
        (reduce (fn [existing-answers answer]
                  ;; TODO: can there be a mismtach when multiple questions?
                  (if (= 2 (count (statement-atom answer)))
                    (update-in existing-answers [(statement-predicate answer)] conj answer)
                    (update-in existing-answers
                               [[(statement-predicate answer) (second (statement-atom answer))]]
                               conj
                               answer)))
                existing-answers
                answers)]
    (assoc service-data :answers existing-answers)))

(defn already-answered?
  [question service-data]
  (let [answers (:answers service-data)]
    (if (or (variable? (second (statement-atom question)))
            (= 2 (count (statement-atom question))))
      (contains? answers (statement-predicate question))
      (contains? answers [(statement-predicate question) (second (statement-atom question))]))))

(defn get-answer
  [question service-data]
  (let [{:keys [state answers]} service-data
        pred (statement-predicate question)
        subject (second (statement-atom question))]
    (if (or (variable? (second (statement-atom question)))
            (= 2 (count (statement-atom question))))
      ;; question of type (pred ?x ...)
      (let [answers (get answers pred)
            ;; TODO: take all answers?
            answer (first answers)]
        (when (nil? answer)
          (throw (Exception. (format "no answer found (%s, %s)" question answers))))
        (reply state question answer))
      ;; question of type (pred subj ...)
      (let [answers (get answers [pred subject])
            answer (first answers)]
        (when (nil? answer)
          (throw (Exception. (format "no answer found (%s, %s)" question answers))))
        (reply state question answer)))))

