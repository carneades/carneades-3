;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns ^{:doc "Utilities functions to manipulate sequences"}
  carneades.editor.utils.seq
  (:require [clojure.zip :as zip]))

(defn reverse-map
  "reverses a bijective map"
  [m]
  (apply hash-map (mapcat (fn [[k v]] [v k]) m)))

(defn zipper-seqloc
  "Returns a lazy sequence of the locations of the zipper"
  [loc]
  (lazy-seq
   (if (zip/end? loc)
     ()
     (cons loc (zipper-seqloc (zip/next loc))))))