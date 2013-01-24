;;; Copyright (c) 2013 Fraunhofer Gesellschaft
;;; Licensed under the EUPL V.1.1

(ns ^{:doc "Reconstructs statements and weights from users' answers."}
  impact.web.controllers.reconstruction
  (:use [carneades.engine.unify :only [apply-substitutions]]
        [carneades.engine.policy :only [get-main-issue policies]]
        [carneades.engine.statement :only [neg literal-predicate variable? literal-atom variables]]
        [impact.web.logic.questions :only [get-predicate get-questions-for-answers-modification
                                           modify-statements-weights]]
        [carneades.engine.dialog :only [get-nthquestion add-answers]])
   (:require [carneades.engine.scheme :as scheme]))

(defn reconstruct-yesno-answer
  "Returns the vector representing the user's response for a yes/no question"
  [values statement]
  (let [value (condp = (first values)
                "yes" 1.0
                "no" 0.0
                "maybe" 0.5)]
   [statement value]))

(defn reconstruct-predicate-answer
  "Returns the vector representing the user's response for a predicate"
  [values statement]
  (let [vars (variables statement) 
        subs (apply hash-map (interleave vars values))]
   [(apply-substitutions subs statement) 1.0]))

(defn reconstruct-role-answer
  [values statement]
  (let [[s o v] statement
        value (first values)]
    [(list s o (symbol (first values))) 1.0]))

(defn reconstruct-answer
  [question theory values]
  (let [statement (:statement question)]
   (cond (:yesnoquestion question)
         (reconstruct-yesno-answer values statement)
         (scheme/role? (get-predicate statement theory))
         (reconstruct-role-answer values statement)
         :else
         (reconstruct-predicate-answer values statement))))

(defn reconstruct-answers
  "Reconstructs the answer from the JSON"
  [jsonanswers dialog theory]
  (reduce (fn [questions-to-weight answer]
            (let [id (:id answer)
                  values (:values answer)
                  question (get-nthquestion dialog id)
                  ans (reconstruct-answer question theory values)
                  [statement value] ans]
              (assoc questions-to-weight statement value)))
          {}
          jsonanswers))

(defn array->stmt
  ([a acc]
     (if (coll? a)
       (cons (array->stmt (first a)) (map array->stmt (rest a)))
       (symbol a)))
  ([a]
     (array->stmt a ())))

(defn reconstruct-statement
  [fact]
  (assoc fact
    :statement (array->stmt (:statement fact))))

(defn reconstruct-statements
  [facts]
  (map reconstruct-statement facts))

