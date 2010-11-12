
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
                             ;(println "depth of " s (depth-in ag n))
                             (depth-in ag n)))
                         p))]
      ;(println "getting position depth" p sum)
      sum)
    ))

(defn sort-by-depth
  [ag]
  (fn [label]
    (sort-by (position-depth ag) label)))

