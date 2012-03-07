(ns impact.web.logic.evaluation
  (:use clojure.pprint
        impact.web.core
        carneades.engine.statement
        carneades.database.export
        (carneades.engine caes argument-evaluation argument-graph statement scheme argument-graph))
  (:require [carneades.database.db :as db]))

(defn get-policies
  [questionid theory]
  (:sections (first (filter #(= (:id %) questionid) (-> theory :sections )))))

(defn get-policy
  [id policies]
  (first (filter #(= (:id %) id) policies)))

(defn get-policy-premises
  [policies]
  )

(defn get-policy-statements
  [policy]
  (filter #(= (term-functor %) 'valid)
   (mapcat #(map (fn [p] (literal-atom (:statement p))) (:premises %)) (:schemes policy))))

(defn evaluate-policy
  [policyid session]
  ;; 1) get the graph from the db
  ;; 2) reject all valid statements not part of the policy
  ;; 3) accept all valid statements that are part of the policy
  ;; 4) save the graph
  ;; 5) change the db field of the session
  (prn "[evaluate-policy] policyid =" policyid)
  (prn "[evaluate-policy] db = " (:db session))
  (let [ag (load-ag (:db session))
        theory (:theory session)
        policies (get-policies 'Q12 theory) ;; TODO get the question id
        policiesid (set (map :id policies))
        policycontent (get-policy policyid policies)
        statements-to-accept (get-policy-statements policycontent)
        statements-to-reject (mapcat get-policy-statements (map #(get-policy % policies) (disj policiesid policyid)))
        _ (prn "[evaluate-policy] to-accept =" statements-to-accept)
        _ (prn "[evaluate-policy] to-reject = " statements-to-reject)
        ag (reject ag statements-to-reject)
        ag (accept ag statements-to-accept)
        ag (enter-language ag (-> session :theory :language))
        ag (evaluate carneades-evaluator ag)
        _ (prn "[evaluate-policy] storing...")
        ;; _ (pprint ag)
        dbname (store-ag ag)
        ]
    (prn "[evaluate-policy] dbname = " dbname)
    dbname
    ))

;; (defn str-to-validstmt
;;   [s]
;;   (list 'valid (symbol s)))

;; OLD:
;; (defn- abduction-positions
;;   [ag acceptability]
;;   (throw (Exception. "NYI (abduction)"))
;;   ;; (condp acceptability =
;;   ;;   :inin (statement-in-label ag (assume-decided-statements ag)
;;   ;;                             (:main-issue ag))
;;   ;;   :inout (statement-out-label ag (assume-decided-statements ag)
;;   ;;                               (:main-issue ag))
;;   ;;   :outin (statement-in-label ag (assume-decided-statements ag)
;;   ;;                              (statement-complement (:mainissue ag)))
;;   ;;   :outout (statement-out-label ag (assume-decided-statements ag)
;;   ;;                                (statement-complement (:main-issue ag))))
;;   )


;; (defn get-stmt-to-ids
;;   [dbname]
;;   (let [content (export-to-argument-graph dbname)
;;         ag (first (:ags content))]
;;     (reduce (fn [stmt-to-id stmt]
;;               (assoc stmt-to-id stmt (:id stmt)))
;;             {}
;;             (:statements-nodes ag)
;;             ;; (map get-statement-node (get-nodes ag))
;;             )))
