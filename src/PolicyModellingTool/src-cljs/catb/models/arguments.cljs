(ns catb.models.arguments
  (:use [jayq.util :only [log clj->js]])
  (:require [clojure.set :as set]
            [catb.models.similarity :as similarity]))

(defn- update-statements
  "Adds all the id of the premises to the statements
indexed by the sources"
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