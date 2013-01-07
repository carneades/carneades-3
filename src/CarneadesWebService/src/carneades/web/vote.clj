;;; Copyright (c) 2011 Fraunhofer Gesellschaft
;;; Licensed under the EUPL V.1.1

(ns ^{:doc "Code to calculate votes statitics"}
  carneades.web.vote
   (:use carneades.database.db))

(defn db-has-main-issue
  "Returns true if the database has the given main issue defined by this atom."
  [dbname atom]
  (try
    (with-db (make-database-connection dbname "guest" "")
      (contains? (set (map :atom (main-issues))) atom))
    (catch Exception _
      (do
        (prn "error with" dbname)
        nil))))

(defn fetch-databases-by-main-issue
  "Returns the list of the databases names having
for main issue the statement with the given atom."
  [atom]
  (filter (fn [dbname] (db-has-main-issue dbname atom)) (fetch-databases-names)))


(defn vote-for-main-issue
  "Returns the vote score for the first main issue
or nil if no vote"
  [dbname]
  (with-db (make-database-connection dbname "root" "pw1")
    (when-let [votes (read-statement-poll "vote-from-argument-page")]
      (let [id (str (:id (first (main-issues))))]
        (get-in votes [:votes id])))))

(defn vote-stats
  "Returns the statistics for the vote on an issue"
  [dbname]
  (with-db (make-database-connection dbname "guest" "")
   (let [main-issue (first (main-issues))
         atom (:atom main-issue)
         dbnames (fetch-databases-by-main-issue atom)
         votes (keep #(vote-for-main-issue %) dbnames)
         nb-votes (count votes)
         grouped-votes (group-by identity votes)]
     {:accepted (/ (count (grouped-votes 1.0)) nb-votes)
      :rejected (/ (count (grouped-votes 0.0)) nb-votes)
      :undecided (/ (count (grouped-votes 0.5)) nb-votes)})))
