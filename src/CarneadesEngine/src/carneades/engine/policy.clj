(ns carneades.engine.policy
  (:use (carneades.engine statement scheme argument-graph aspic argument-evaluation utils)
        [clojure.tools.logging :only (info debug error)])
  (:require [carneades.config.reader :as config]))

(def policies-directory (config/properties "policies-directory"))
(def policies-metadata (read-string
                        (slurp
                         (str policies-directory file-separator "policies.clj"))))

(defn get-policy-filename
  [filename]
  (str policies-directory file-separator filename))

(def policies (reduce (fn [policies metadata]
                        (let [{:keys [filename namespace name]} metadata]
                          (assoc policies name
                                 (load-theory (get-policy-filename filename) namespace name))))
                      {}
                      policies-metadata))

(defn get-policies
  [questionid theory]
  (:sections (first (filter #(= (:id %) questionid) (-> theory :sections)))))

(defn get-policy
  [id policies]
  (first (filter #(= (:id %) id) policies)))

(defn get-policy-statements
  [policy]
  (filter #(= (term-functor %) 'valid)
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
        ag (enter-language ag (:language theory))
        ag (evaluate aspic-grounded ag)]
    ag))

(defn get-main-issue [theory qid]
  (:main-issue (first (filter #(= (:id %) qid) (-> theory :sections)))))

(defn give-acceptability?
  "Returns true if the given policy would give the issueid this particular acceptability."
  [ag issueid acceptability policy]
  (let [policy-statements (get-policy-statements policy)
        _ (debug "policy-statements =" policy-statements)
        ag (accept ag policy-statements)
        ag (evaluate aspic-grounded ag)
        _ (debug "issueid:" issueid)
        _ (debug "issue type:" (type issueid))
        _ (debug "stmt keys:" (keys (:statement-nodes ag)))
        _ (debug "type of second key:" (type (second (keys (:statement-nodes ag)))))
        issue-stmt (get-in ag [:statement-nodes issueid])]
    (debug "issue-stmt=" issue-stmt)
    (condp = acceptability
      :in (in-node? issue-stmt)
      :out (out-node? issue-stmt)
      :undecided (undecided-node? issue-stmt))))

(defn find-policies
  "Returns the policies in the theory that, if accepted, would give the issueid
   the given acceptability. Acceptability is either :in :out or :undecided"
  [ag theory qid issueid acceptability]
  (debug "find-policies")
  (let [policies (get-policies qid theory)
        selected-policies
        (filter (partial give-acceptability? ag issueid acceptability) policies)]
    selected-policies))