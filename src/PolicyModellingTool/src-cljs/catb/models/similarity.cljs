;;; Copyright (c) 2012 Fraunhofer Gesellschaft
;;; Licensed under the EUPL V.1.1

(ns catb.models.similarity
  (:require [clojure.set :as set]))

(defn score
  "Returns the similarity score of two sets."
  [s1 s2]
  (/ (count (set/intersection s1 s2))
     (count s2)))

(defn value
  "Returns the similarity value of two sets.
Either :very-much :much :some :little or :very-little"
  [s1 s2]
  (let [s (score s1 s2)]
    (cond (and (>= s 0.0) (<= s 0.2)) :very-little
          (and (> s 0.2) (<= s 0.4)) :little
          (and (> s 0.4) (<= s 0.6)) :some
          (and (> s 0.6) (<= s 0.8)) :much
          (and (> s 0.8) (<= s 1.0)) :very-much)))