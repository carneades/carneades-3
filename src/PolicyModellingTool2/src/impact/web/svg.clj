(ns impact.web.svg
  (:use clojure.data.json
        ;; clojure.java.io
        carneades.engine.lkif
        carneades.engine.statement
        carneades.mapcomponent.export
        [clojure.java.io :only (input-stream reader)])
  (:import java.io.File))

(defn output-svg
  [pathname ]
  (let [lkif (import-lkif pathname)
        ;; {:keys [layout treeify radius depth]} (keywordify params)
        layout :hierarchical
        treeify false
        depth Integer/MAX_VALUE
        radius 90
        pa (merge {:layout layout}
                  (if (nil? treeify)
                    nil
                    {:treeify (Boolean/valueOf treeify)})
                  (if (nil? radius)
                    nil
                    {:radius (Integer/valueOf radius)})
                  (if (nil? depth)
                    nil
                    {:depth (Integer/valueOf depth)}))
        ;; _ (do (prn "pa =") (prn pa))
        svg (apply export-ag-os (first (:ags lkif)) statement-formatted (flatten (map identity pa)))]
    (apply str (line-seq (reader svg)))))

(defn process-ajax-request
  [uri session params]
  (prn "uri =")
  (prn uri)
  (let [pathname (str "/tmp/" (.getName (File. uri)))]
    (prn "path =")
    (prn pathname)
    {:headers {"Content-Type" "text/xml;charset=UTF-8"}
     :body (output-svg pathname)}))

