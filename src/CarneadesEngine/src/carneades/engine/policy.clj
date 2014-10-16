;; Copyright (c) 2012 Fraunhofer Gesellschaft
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns carneades.engine.policy
  (:use (carneades.engine statement theory argument-graph aspic argument-evaluation utils))
  (:require [carneades.engine.translation :as tr]
            [carneades.engine.theory.translation :as ttr]
            [taoensso.timbre :as timbre :refer [debug info]]))

(defn get-policies
  [questionid theory]
  (:sections (first (filter #(= (:id %) questionid) (-> theory :sections)))))

(defn get-policy
  [id policies]
  (first (filter #(= (:id %) id) policies)))

(defn get-policy-statements
  [policy]
  (filter #(= (term-functor %) 'is-valid)
   (mapcat #(map (fn [p] (literal-atom (:statement p))) (:premises %)) (:schemes policy))))

(defn evaluate-policy
  [qid policyid theory ag]
  (let [policies (get-policies qid theory)
        policiesid (set (map :id policies))
        policycontent (get-policy policyid policies)
        statements-to-accept (get-policy-statements policycontent)
        other-policies (map #(get-policy % policies) (disj policiesid policyid))
        statements-to-reject (mapcat get-policy-statements other-policies)
        ag (reject ag statements-to-reject)
        ag (accept ag statements-to-accept)
        ag (tr/translate-ag ag (comp (tr/make-default-translator)
                                     (ttr/make-language-translator (:language theory))))
        ag (evaluate aspic-grounded ag)]
    ag))

(defn get-main-issue [theory qid]
  (:main-issue (first (filter #(= (:id %) qid) (-> theory :sections)))))

(defn give-acceptability?
  "Returns true if the given policy would give the issueid this particular acceptability."
  [ag theory qid issueid acceptability policy]
  (let [ag (evaluate-policy qid (:id policy) theory ag)
        issue-stmt (get-in ag [:statement-nodes issueid])]
    (debug "issue-stmt=" issue-stmt)
    (condp = acceptability
      :in (in-node? issue-stmt)
      :out (out-node? issue-stmt)
      :undecided (undecided-node? issue-stmt))))

(defn find-policies
  "Returns the policies' ids in the theory that, if accepted, would give the issueid
   the given acceptability. Acceptability is either :in :out or :undecided"
  [ag theory qid issueid acceptability]
  (debug "find-policies")
  (let [policies (get-policies qid theory)
        selected-policies
        (filter (partial give-acceptability? ag theory qid issueid acceptability) policies)]
    (map :id selected-policies)))
