(ns impact.web.logic.evaluation
  (:use clojure.pprint
        impact.web.core
        carneades.database.export
        (carneades.engine policies caes argument-evaluation argument-graph statement scheme argument-graph))
  (:require [carneades.database.db :as db]))

(defn evaluate-policy
  [qid policyid session]
  ;; 1) get the graph from the db
  ;; 2) reject all valid statements not part of the policy
  ;; 3) accept all valid statements that are part of the policy
  ;; 4) save the graph
  ;; 5) change the db field of the session
  (prn "[evaluate-policy] policyid =" policyid)
  (prn "[evaluate-policy] db = " (:db session))
  (let [ag (load-ag (:db session))
        theory (:theory session)
        policies (get-policies qid theory)
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
