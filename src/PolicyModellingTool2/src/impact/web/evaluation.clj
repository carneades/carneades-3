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

(defn str-to-validstmt
  [s]
  (list 'valid (symbol s)))

(defn evaluate-graph
  [pathname accepted-stmts rejected-stmts]
  (prn "pathname =")
  (prn pathname)
  (let [ag (-> (first (:ags (import-lkif pathname)))
               (accept (map str-to-validstmt accepted-stmts))
               (reject (map str-to-validstmt rejected-stmts)))]
    (store-ag ag)))