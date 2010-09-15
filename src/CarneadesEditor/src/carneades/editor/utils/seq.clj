;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.editor.utils.seq)

(defn reverse-map [m]
  (apply hash-map (mapcat (fn [[k v]] [v k]) m)))
