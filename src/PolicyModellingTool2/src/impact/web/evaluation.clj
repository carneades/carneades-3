(ns impact.web.evaluation
  (:use impact.web.core)
  (:import java.io.File))

(defn show-graph
  [pathname]
  (if *debug*
    (str "/svg/" (.getName (File. pathname)))
    (str "/PolicyModellingTool2/svg/" (.getName (File. pathname)))))

