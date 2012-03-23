(ns carneades.engine.policy
  (:use (carneades.engine statement scheme argument-graph caes argument-evaluation))
  (:require [carneades.config.reader :as config]))

(def default-policies-file (config/properties "policies-file"))
(def default-policies-namespace  (config/properties "policies-namespace"))
(def default-policies-name  (config/properties "policies-name"))

;; (def default-policies (load-theory default-policies-file
;;                                    (symbol default-policies-namespace)
;;                                    (symbol default-policies-name)))

(printf "[policy] file = %s\n[policy] namespace = %s\n[policy] name = %s\n" default-policies-file default-policies-namespace default-policies-name)

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
        ag (evaluate carneades-evaluator ag)]
    ag))