(ns carneades.engine.policy
  (:use (carneades.engine statement scheme argument-graph aspic argument-evaluation))
  (:require [carneades.config.reader :as config]))

(def impact-policies-file (config/properties "impact-policies-file"))
(def impact-policies-namespace  (config/properties "impact-policies-namespace"))
(def impact-policies-name  (config/properties "impact-policies-name"))

(printf "\n[policy] file = %s\n[policy] namespace = %s\n[policy] name = %s\n" impact-policies-file impact-policies-namespace impact-policies-name)

(defn get-policies
  [questionid theory]
  (:sections (first (filter #(= (:id %) questionid) (-> theory :sections )))))

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
        _ (prn "[evaluate-policy] to-accept =" statements-to-accept)
        _ (prn "[evaluate-policy] to-reject = " statements-to-reject)
        ag (reject ag statements-to-reject)
        ag (accept ag statements-to-accept)
        ag (enter-language ag (:language theory))
        ag (evaluate aspic-grounded ag)]
    ag))

(defn get-main-issue [theory qid]
  (:main-issue (first (filter #(= (:id %) qid) (-> theory :sections)))))