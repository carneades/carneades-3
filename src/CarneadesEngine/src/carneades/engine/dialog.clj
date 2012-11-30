(ns ^{:doc "Save questions and answers of the dialog between the engine and the user (when using ask.clj)"}
  carneades.engine.dialog
  (:use clojure.pprint
        carneades.engine.statement
        [carneades.engine.unify :only (unify genvar apply-substitutions)])
  (:require [carneades.engine.scheme :as scheme]))

(defrecord Dialog [questions answers])

(defn add-questions
  "Add questions to the dialog history"
  [dialog questions]
  (update-in dialog [:questions] concat questions))

(defn replace-map
  [question]
  (reduce (fn [m v]
            (assoc m v (genvar)))
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

(defn previous-answers
  "Returns the previous answers for this question in the dialog"
  [question dialog]
  (filter (fn [k] (unify question k)) (keys (:answers dialog))))

(defn replace-sliteral-value
  "Replaces the value of a binary sliteral by another value"
  [role value]
  (let [[subject object _] role]
    (list subject object value)))

(defn get-answers
  "Return a sequence of atomic answers for a given questions or nil if no answers.
   Note: an empty sequence can be returned, it does mean that there were answers
   but they were answered with 'maybe'."
  [dialog theory question]
  {:pre [(do (prn "                    [get-answers] " question) true)]
   :post [(do (prn "                    ====> " %) true)]}
  (if-let [key (first (previous-answers question dialog))]
    (get-in dialog [:answers key])
    ;; if
    ;; * we don't have an answer and
    ;; * the question is grounded and
    ;; * corresponds to a functional (min=1, max=1) role predicate in the theory and
    ;; * the possible values (the type) are expressed in a set
    ;; * and we have an answer for one of the other possible values in the dialog
    ;; then the response is the negation of the question
    (when-let [pred (get-in theory [:language (literal-predicate question)])]
      (when (and (ground? question)
                 (scheme/role? pred)
                 (= (:max pred) 1)
                 (= (:max pred) 1)
                 (set? (:type pred))
                 (seq (mapcat #(previous-answers (replace-sliteral-value question %) dialog)
                              (disj (:type pred) (second (term-args question))))))
        [(literal-complement question)]))))

(defn get-nthquestion
  "Returns the nth questions of the dialog history"
  [dialog n]
  (first (filter (fn [q] (= (:id q) n)) (:questions dialog))))

(defn make-dialog
  "Creates a new dialog"
  []
  (->Dialog () {}))