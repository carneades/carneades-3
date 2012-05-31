(ns carneades.engine.policy
  (:use (carneades.engine statement scheme argument-graph aspic argument-evaluation utils))
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