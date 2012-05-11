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
  [dialog questions answers]
  {:pre [(>= (count answers) 1)
         (seq? questions)]}
  (reduce (fn [dialog [question answer]]
            (prn "question -> answer")
            (prn question)
            (prn answer)
            (update-in dialog [:answers (answer-key question)] conj answer))
          dialog
          (partition 2 (interleave questions answers))))

(defn get-answers
  "Return a sequence of atomic answers for a given questions"
  [dialog question]
  ;; (prn "[get-answers] dialog =")
  ;; (pprint dialog)
  (let [question (:atom (positive-statement question))]
    ;; (prn "[get-answers] question =" question)
    (when-let [key (first (filter (fn [k] (unify question k)) (keys (:answers dialog))))]
      ;; (prn "key = " key)
      (get-in dialog [:answers key]))))

(defn get-nthquestion
  "Returns the nth questions of the dialog history"
  [dialog n]
  (first (filter (fn [q] (= (:id q) n)) (:questions dialog))))

(defn make-dialog
  "Creates a new dialog"
  []
  (->Dialog () {}))