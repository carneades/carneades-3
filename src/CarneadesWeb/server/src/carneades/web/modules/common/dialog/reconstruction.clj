;; Copyright (c) 2013 Fraunhofer Gesellschaft
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns ^{:doc "Reconstructs statements and weights from users' answers."}
  carneades.web.modules.common.dialog.reconstruction
  (:require [carneades.engine.theory :as scheme]
            [carneades.engine.unify :refer [apply-substitutions]]
            [carneades.engine.policy :refer [get-main-issue]]
            [carneades.engine.statement
             :refer [neg 
                    literal-predicate 
                    variable? 
                    literal-atom 
                    variables]]
            [carneades.web.modules.common.dialog.questions
             :refer [get-predicate
                    get-questions-for-answers-modification
                    modify-statements-weights]]
            [carneades.engine.dialog :refer [get-nthquestion add-answers]]))

(defn reconstruct-grounded-answer
  "Returns a collection of vector representing the user's responses for a yes/no question"
  [value statement]
  [[statement (condp = value
                "yes" 1.0
                "no" 0.0
                "maybe" 0.5)]])

(defn reconstruct-predicate-answer
  "Returns the vector representing the user's response for a predicate"
  [values statement]
  (let [vars (variables statement) 
        subs (apply hash-map (interleave vars values))]
   [(apply-substitutions subs statement) 1.0]))

(defn reconstruct-role-answer
  [coll-of-values statement]
  (map (fn [values]
         (let [[s o v] statement
               value (first values)]
           [(list s o (symbol value)) 1.0]))
       coll-of-values))

(defn reconstruct-answer
  [question theory answer]
  (let [statement (:statement question)]
   (cond (:grounded question)
         (reconstruct-grounded-answer (:value answer) statement)
         
         (scheme/role? (get-predicate statement theory))
         (reconstruct-role-answer (:values answer) statement)
         
         :else
         ;; still implemented??
         (reconstruct-predicate-answer (:values answer) statement))))

(defn reconstruct-answers
  "Reconstructs the answer from the JSON.
Returns a list of [statement weight]."
  [jsonanswers dialog theory]
  (mapcat (fn [answer]
            (let [id (:id answer)
                  question (get-nthquestion dialog (:id answer))]
              (reconstruct-answer question theory answer)))
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

