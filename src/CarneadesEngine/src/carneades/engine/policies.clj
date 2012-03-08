(ns carneades.engine.policies
  (:use carneades.engine.statement))

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
