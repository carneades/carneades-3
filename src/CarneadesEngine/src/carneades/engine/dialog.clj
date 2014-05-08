;; Copyright (c) 2012 Fraunhofer Gesellschaft
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns ^{:doc "Save questions and answers of the dialog between the
  engine and the user (when using ask.clj). 

  Essentially, the ask component provides a tool for the user to define
  a table for each askable predicate. Each row in the table represents
  a statement node, including its weight, where weight=1.0 if the
  statement has been accepted as true, 0.0 if the statement has been
  rejected as false, and 0.5 otherwise. Notice that negative
  statements can be included in the table, by setting their weights to
  0.0.

  The ask component returns a sequence of responses, implementing the
  ArgumentGenerator protocol. If the weight of the statement is 1.0 in
  the table, a singelton response is returned, with the statement as a
  positive assumption. The the weight of the statement in the table is
  0.0, a singleton response is returned, with the negation of the
  statement as an assumption. Otherwise the query fails by returning an
  empty sequence. "}
  carneades.engine.dialog
  (:use clojure.pprint
        carneades.engine.statement
        [carneades.engine.unify :only (unify genvar apply-substitutions)])
  (:require [carneades.engine.theory :as scheme]
            [taoensso.timbre :as timbre :refer [debug info]]))

(defrecord Dialog [questions answers])

(defn add-questions
  "Add questions to the dialog history"
  [dialog questions]
  (update-in dialog [:questions] concat questions))

(defn replace-map
  "Builds a replacement map. This is used
  to replace variable by fresh variables when indexing the statement.
  This prevents unification errors when querying for an answer."
  [question]
  (reduce (fn [m v]
            (assoc m v (genvar)))
          {}
          (variables question)))

(defn answer-key
  [question]
  {:pre [(sliteral? question)]
   :post [(not (instance? clojure.lang.LazySeq %))]}
  (apply list (replace (replace-map question) (:atom (positive-statement question)))))

(defn xconj
  "Like clojure.core/conj but creates a set if s is nil."
  [s x]
  (if (nil? s)
    #{x}
    (conj s x)))

(defn add-answers
  "Add answers to the dialog for the given atomic questions."
  [dialog questions-to-weight]
  {:pre [(coll? questions-to-weight)]
   :post [(do (info "add-answers =>") (pprint %) (prn "================================") true)]}
  (reduce (fn [dialog [question weight]]
            (update-in dialog [:answers (answer-key question)] xconj weight))
          dialog
          questions-to-weight))

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
    (let [answers (get-in dialog [:answers key])]
      (cond (answers 1.0) (list question)
            (answers 0.0) (list (neg question))
            :else ()))
    ;; if
    ;; * we don't have an answer and
    ;; * the question is grounded and
    ;;;; * corresponds to a functional (min=1, max=1) role predicate in the theory and
    ;; * the possible values (the type) are expressed in a set
    ;; * and we have an answer for one of the other possible values in the dialog
    ;; then the response is the negation of the question

    ;; here we try a closed-world assumption
    (when-let [pred (get-in theory [:language (literal-predicate question)])]
      (when (and (ground? question)
                 (scheme/role? pred)
                 ;; (= (:max pred) 1)
                 ;; (= (:max pred) 1)
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
