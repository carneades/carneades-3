(ns ^{:doc "Storage for questions already answered by the user."}
  impact.web.logic.answers
  (:use clojure.pprint
        carneades.engine.statement
        [carneades.engine.ask :only (make-answer)]))

(defn add-answers
  "Stores answers and index them on their predicate."
  [session answers]
  {:pre [(not (nil? session))]}
  (let [existing-answers (:answers session)
        existing-answers
        (reduce (fn [existing-answers answer]
                  (assoc existing-answers (literal-predicate answer) answer))
                existing-answers
                answers)]
    (assoc session :answers existing-answers)))

(defn already-answered?
  "Returns true if the question has already been answered by the user."
  [question session]
  (prn "[answers] answers =" (:answers session))
  (let [answers (:answers session)]
    (contains? answers (literal-predicate question))))

(defn get-answer
  [question session]
  (prn "[get-answer] question = " question)
  (let [{:keys [substitutions answers]} session
        _ (prn "[get-answer] substitutions =" substitutions)
        _ (prn "[get-answers] answers = " answers)
        pred (literal-predicate question)
        answer (get answers pred)
        [subs response] (make-answer substitutions question answer)]
    [subs response]))



