(ns catb.models.core
  (:use [jayq.util :only [log clj->js]])
  (:require [clojure.set :as set]
            [catb.models.similarity :as similarity]))

(defn get-stmt
  "Returns the statement model."
  [statements id]
  (.get statements id))

(defn get-arg
  "Returns the argument model."
  [args id]
  (.get args id))

(defn- update-statements
  "Adds all the id of the premises to the statements
indexed by the sources" ;; TODO: add conclusions
  [m sources premises]
  (let [ids (set (map (fn [p] (.-id (.-statement p))) premises))]
    (reduce (fn [m source]
              (if (contains? m source)
                (update-in m [source]
                           set/union
                           ids)
                (assoc m source ids)))
            m
            (js->clj sources))))

(defn statements-by-sources
  "Index all the statements by their sources.
   Statements without arguments are ignored.
   Returns a map source -> (set-of statements-ids).
   Args is a sequence of JSON arguments"
  [args]
  (reduce (fn [m arg]
            (if (and (.-header arg) (.-source (.-header arg)))
              (let [sources (.-source (.-header arg))
                    premises (.-premises arg)]
                (update-statements m sources premises))
              m))
          {} args))

(defn sources-by-similarity
  "Index all sources by similarity.
Returns a map similarity-score -> (set-of source)
statements is map source -> (set-of statements-ids)
accepted is the set of the accepted statements' ids"
  [statements accepted]
  (reduce (fn [index [source ids]]
            (log "ids")
            (log (clj->js ids))
            (log "accepted")
            (log (clj->js accepted))
            (let [v (similarity/value accepted ids)]
              (log "value for")
              (log source)
              (log "is")
              (log v)
              (if (contains? index v)
                (update-in index [v] conj source)
                (assoc index v [source]))))
             {} statements))

(defn arguments-for-statement
  "Returns all descendant arguments models for a given JSON statement
as a sequence."
  [issue args]
  (let [pro-statements
        (mapcat (fn [proid]
                  (map (fn [p]
                         (.-statement p))
                       (.get (.get args proid) "premises")))
                (.-pro issue))
        con-statements
        (mapcat (fn [conid]
                  (map (fn [p]
                         (.-statement p))
                       (.get (.get args conid) "premises")))
                (.-con issue))]
    (concat (map (partial get-arg args) (.-pro issue))
            (map (partial get-arg args) (.-con issue))
            (mapcat (fn [s] (arguments-for-statement s args)) pro-statements)
            (mapcat (fn [s] (arguments-for-statement s args)) con-statements))))