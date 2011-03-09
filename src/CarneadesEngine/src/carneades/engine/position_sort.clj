;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns ^{:doc "Various helper functions to sort positions when doing abductive reasoning."}
  carneades.engine.position-sort
  (:use clojure.contrib.def
        carneades.engine.argument))

;;; confusion between depth and height meaning?
;;; (see Mantis bug tracker and the position Assistant implementation)

(defn position-depth "Returns the depth of a position in an argument graph"
  [ag p]
  (apply + (map (fn [s]
                  (let [n (get-node ag s)]
                    (depth-in ag n)))
                p)))

(defn position-height
  "Returns the height of a position in an argument graph"
  [ag p]
  (apply + (map
            (fn [s]
              (let [n (get-node ag s)]
                (height-in ag n)))
            p)))
