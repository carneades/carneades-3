;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.editor.utils.seq
  (:require [clojure.zip :as zip]))

(defn reverse-map [m]
  "reverses a bijective map"
  (apply hash-map (mapcat (fn [[k v]] [v k]) m)))

(defn zipper-seqloc [loc]
  "Returns a lazy sequence of the locations of the zipper"
  (lazy-seq
   (if (zip/end? loc)
     ()
     (cons loc (zipper-seqloc (zip/next loc))))))