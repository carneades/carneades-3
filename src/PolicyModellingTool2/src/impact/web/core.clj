(ns impact.web.core
  (:use carneades.engine.lkif)
  (:import java.io.File))

(def ^{:dynamic false} *debug* true)


(defn store-ag
  "Stores ag into a LKIF and returns the pathname of the file"
  [ag]
  (let [pathname (.getPath (File/createTempFile "graph" ".lkif"))]
    (export-lkif (assoc *empty-lkif* :ags [ag]) pathname)
    pathname))
