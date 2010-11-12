
(ns carneades.engine.position-sort
  ;(:require )
  (:use 
    clojure.contrib.def
    carneades.engine.argument
    )
  ;(:import )
  )

(defn- position-depth
  [ag]
  (fn [p]
    (let [sum (apply + (map 
                         (fn [s]
                           (let [n (get-node ag s)]
                             (depth-in ag n)))
                         p))]
      sum)))

(defn- position-height
  [ag]
  (fn [p]
    (let [sum (apply + (map
                         (fn [s]
                           (let [n (get-node ag s)]
                             (height-in ag n)))
                         p))]
      sum)))

(defn sort-by-depth
  [ag]
  (fn [label]
    (sort-by (position-depth ag) label)))

(defn sort-by-height
  [ag]
  (fn [label]
    (sort-by (position-height ag) label)))

