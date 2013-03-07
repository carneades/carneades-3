;;; Copyright (c) 2012 Fraunhofer Gesellschaft
;;; Licensed under the EUPL V.1.1

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

(defn get-metadata
  "Returns the metadata model for a given key"
  [md k]
  (first (filter (fn [m]
                   (= (.-key m) k))
                 (.toJSON md))))

(defn- update-statements
  "Adds all the ids to the statements indexed by the sources."
  [m sources ids]
  (reduce (fn [m source]
            (let [v (get m source #{})]
             (assoc m source (set/union v ids))))
          m
          (js->clj sources)))

(defn statements-by-sources
  "Index all the statements by their sources.
   Statements without arguments are ignored.
   Returns a map source -> (set-of statements-ids).
   Args is a sequence of JSON arguments"
  [args]
  (reduce (fn [m arg]
            (if (and (.-header arg) (.-source (.-header arg)))
              (let [sources (.-source (.-header arg))
                    premises (.-premises arg)
                    conclusion (.-conclusion arg)
                    conclusion-id (.-id conclusion)
                    premises-ids (set (map (fn [p] (.-id (.-statement p))) premises))
                    ids (if (.-pro arg)
                          (conj premises-ids conclusion-id)
                          premises-ids)]
                (update-statements m sources ids))
              m))
          {} args))

(defn sources-by-similarity
  "Indexes all sources by similarity.
Returns a map similarity-score -> (set-of source)
statements is map source -> (set-of statements-ids)
accepted is the set of the accepted statements' ids"
  [statements accepted]
  (group-by (fn [source]
              (similarity/value accepted (statements source)))
            (keys statements)))

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