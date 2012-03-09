(ns impact.web.logic.evaluation
  (:use clojure.pprint
        impact.web.core
        carneades.database.export
        (carneades.engine caes argument-evaluation argument-graph statement scheme argument-graph))
  (:require [carneades.database.db :as db]
            [carneades.engine.policy :as policy]))

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
        ag (policy/evaluate-policy qid policyid theory ag)
        _ (prn "[evaluate-policy] storing...")
        dbname (store-ag ag)]
    (prn "[evaluate-policy] dbname = " dbname)
    dbname))

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
