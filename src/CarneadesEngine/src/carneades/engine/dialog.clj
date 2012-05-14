(ns ^{:doc "Save questions and answers of the dialog between the engine and the user (when using ask.clj)"}
  carneades.engine.dialog
  (:use clojure.pprint
        carneades.engine.statement
        [carneades.engine.unify :only (unify apply-substitutions)]))

(defrecord Dialog [questions answers])

(defn add-questions
  "Add questions to the dialog history"
  [dialog questions]
  (update-in dialog [:questions] concat questions))

(defn replace-map
  [question]
  (reduce (fn [m v]
            (assoc m v '?_))
          {}
          (variables question)))

(defn answer-key
  [question]
  {:pre [(sliteral? question)]}
  (replace (replace-map question) (:atom (positive-statement question))))

(defn add-answers
  "Add answers to the dialog for the given atomic questions"
  [dialog questions-to-answers]
  {:pre [(seq? questions-to-answers)]}
  (reduce (fn [dialog [question answer]]
            (prn "question -> answer")
            (prn question)
            (prn answer)
            (if (nil? answer)
              (let [answers (get-in dialog [:answers (answer-key question)])]
                (if (seq answers)
                  dialog
                  (assoc-in dialog [:answers (answer-key question)] ())))
              (update-in dialog [:answers (answer-key question)] conj answer)))
          dialog
          questions-to-answers))

(defn get-answers
  "Return a sequence of atomic answers for a given questions or nil if no answers.
   Note: an empty sequence can be returned, it does mean that there were answers
   but they were answered with 'maybe'."
  [dialog question]
  (let [question (:atom (positive-statement question))]
    (when-let [key (first (filter (fn [k] (unify question k)) (keys (:answers dialog))))]
      (get-in dialog [:answers key]))))

(defn get-nthquestion
  "Returns the nth questions of the dialog history"
  [dialog n]
  (first (filter (fn [q] (= (:id q) n)) (:questions dialog))))

(defn make-dialog
  "Creates a new dialog"
  []
  (->Dialog () {}))