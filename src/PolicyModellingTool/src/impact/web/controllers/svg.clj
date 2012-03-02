(ns impact.web.controllers.svg
  (:use clojure.data.json
        ;; clojure.java.io
        carneades.engine.statement
        carneades.maps.lacij
        carneades.database.export
        [clojure.java.io :only (input-stream reader)])
  (:import java.io.File))

(def *tmpdir* (System/getProperty "java.io.tmpdir"))

(defn output-svg
  [dbname]
  ;; TODO: use the service
  (let [lkif (export-to-argument-graph dbname)
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
        svg (apply export-str (first (:ags lkif)) (flatten (map identity pa)))]
    (apply str (line-seq (reader svg)))))


(defn process-ajax-request
  [uri session params]
  (let [pathname (str *tmpdir* "/" (.getName (File. uri)))]
    {:headers {"Content-Type" "text/xml;charset=UTF-8"}
     :body (output-svg pathname)}))

