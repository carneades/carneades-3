;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.engine.position-sort
  (:use clojure.contrib.def
        carneades.engine.argument))

(defn position-depth [ag p]
  (apply + (map (fn [s]
                  (let [n (get-node ag s)]
                    (depth-in ag n)))
                p)))

(defn position-height [ag p]
  (apply + (map
            (fn [s]
              (let [n (get-node ag s)]
                (height-in ag n)))
            p)))

;; (defn sort-by-depth
;;   [ag]
;;   (fn [label]
;;     (sort-by (position-depth ag) label)))

;; (defn sort-by-height
;;   [ag]
;;   (fn [label]
;;     (sort-by (position-height ag) label)))

