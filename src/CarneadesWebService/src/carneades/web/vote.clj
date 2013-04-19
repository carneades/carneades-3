;;; Copyright (c) 2011 Fraunhofer Gesellschaft
;;; Licensed under the EUPL V.1.1

(ns ^{:doc "Code to calculate votes statitics"}
  carneades.web.vote
  (:use carneades.database.db
        [carneades.engine.policy :only [find-policies]]
        [carneades.database.export :only [export-to-argument-graph]])
  (:require [carneades.database.case :as case]
            [carneades.project.admin :as project]))

(defn vote-stats
  "Returns the statistics for the vote on a case"
  [project debateid casedb]
  (with-db (make-connection project "debates" "guest" "")
   (let [opinions (case/get-opinions-for-case debateid casedb)
         nb-opinions (count opinions)
         nb-opinions (if (zero? nb-opinions) 1 nb-opinions)
         grouped-opinions (group-by identity opinions)]
     {:accepted (/ (count (grouped-opinions 1.0)) nb-opinions)
      :rejected (/ (count (grouped-opinions 0.0)) nb-opinions)
      :undecided (/ (count (grouped-opinions 0.5)) nb-opinions)})))

(defn find-policies-matching-vote
  "Finds the policies that give to the main issue the same acceptability
as the one from the user's vote."
  [project project-properties m]
  {:pre [(not (nil? (:casedb m)))]}
  (let [{:keys [casedb policykey qid issueid opinion]} m
        dbconn (make-connection project casedb "guest" "")]
    (with-db dbconn
      (let [ag (export-to-argument-graph dbconn)
            policy (project/load-theory project (:policies project-properties))]
        (find-policies ag policy (symbol qid) (symbol issueid)
                       (condp = opinion
                         1 :in
                         0 :out
                         0.5 :undecided))))))

(defn aggregated-vote-stats
  "Returns the preferred policies for a given debate"
  [project debateid]
  (with-db (make-connection project "debates" "guest" "")
   (let [nb-polls (case/count-polls-for-debate debateid)
         policies (case/get-policies-for-debate debateid)]
     (reduce (fn [result [policy nb-favorable-opinion]]
               (assoc result policy (/ nb-favorable-opinion nb-polls)))
             {}
             (frequencies policies)))))
