(ns impact.web.evaluation
  (:use impact.web.core
        carneades.engine.lkif
        carneades.engine.argument
        carneades.engine.statement)
  (:import java.io.File))

(defn show-graph
  [pathname]
  (if *debug*
    (str "/svg/" (.getName (File. pathname)))
    (str "/PolicyModellingTool2/svg/" (.getName (File. pathname)))))

(defn get-policies
  [lkifpath]
  (let [content (import-lkif lkifpath)]
    (map (comp second :statement) (get-nodes (first (:ags content)) 'valid))))