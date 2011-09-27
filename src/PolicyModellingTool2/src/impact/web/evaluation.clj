(ns impact.web.evaluation
  (:import java.io.File))

(defn show-graph
  [pathname]
  (str "/svg/" (.getName (File. pathname))))

