;;; Copyright (c) 2011 Fraunhofer Gesellschaft
;;; Licensed under the EUPL V.1.1

(ns ^{:doc "Code to calculate votes statitics"}
  carneades.web.vote
  (:use carneades.database.db
        [carneades.engine.policy :only [policies find-policies]]
        [carneades.database.export :only [export-to-argument-graph]])
  (:require [carneades.database.admin :as admin]))

;; (defn db-has-main-issue
;;   "Returns true if the database has the given main issue defined by this atom."
;;   [dbname atom]
;;   (try
;;     (with-db (make-database-connection dbname "guest" "")
;;       (contains? (set (map :atom (main-issues))) atom))
;;     (catch Exception _
;;       (do
;;         (prn "error with" dbname)
;;         nil))))

;; (defn fetch-databases-by-main-issue
;;   "Returns the list of the databases names having
;; for main issue the statement with the given atom."
;;   [atom]
;;   (filter (fn [dbname] (db-has-main-issue dbname atom)) (fetch-databases-names)))


;; (defn vote-for-main-issue
;;   "Returns the vote score for the first main issue
;; or nil if no vote"
;;   [dbname]
;;   (with-db (make-database-connection dbname "root" "pw1")
;;     (when-let [votes (read-statement-poll "vote-from-argument-page")]
;;       (let [id (str (:id (first (main-issues))))]
;;         (get-in votes [:votes id])))))

(defn vote-stats
  "Returns the statistics for the vote on a case"
  [debateid casedb]
  (with-db (make-database-connection "debates" "guest" "")
   (let [opinions (admin/get-opinions-for-case debateid casedb)
         nb-opinions (count opinions)
         grouped-opinions (group-by identity opinions)]
     {:accepted (/ (count (grouped-opinions 1.0)) nb-opinions)
      :rejected (/ (count (grouped-opinions 0.0)) nb-opinions)
      :undecided (/ (count (grouped-opinions 0.5)) nb-opinions)})))

(defn find-policies-matching-vote
  "Finds the policies that give to the main issue the same acceptability
as the one from the user's vote."
  [m]
  (let [{:keys [casedb policykey qid issueid opinion]} m
        dbconn (make-database-connection casedb "guest" "")]
    (with-db dbconn
      (let [ag (export-to-argument-graph dbconn)
            theory (policies (symbol policykey))]
        (find-policies ag theory (symbol qid) (symbol issueid)
                       (condp = opinion
                         1 :in
                         0 :out
                         0.5 :undecided))))))

(defn aggregated-vote-stats
  "Returns the preferred policies for a given debate"
  [debateid]
  (with-db (make-database-connection "debates" "guest" "")
   (let [nb-polls (admin/count-polls-for-debate debateid)
         policies (admin/get-policies-for-debate debateid)]
     (reduce (fn [result [policy nb-favorable-opinion]]
               (assoc result policy (/ nb-favorable-opinion nb-polls)))
             {}
             (frequencies policies)))))