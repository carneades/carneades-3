(ns impact.web.evaluation
  (:use impact.web.core
        carneades.engine.lkif
        carneades.engine.argument
        carneades.engine.statement
        carneades.engine.abduction)
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

(defn- abduction-positions
  [ag acceptability]
  (condp acceptability =
    :inin (statement-in-label ag (assume-decided-statements ag)
                              (:main-issue ag))
    :inout (statement-out-label ag (assume-decided-statements ag)
                                (:main-issue ag))
    :outin (statement-in-label ag (assume-decided-statements ag)
                               (statement-complement (:mainissue ag)))
    :outout (statement-out-label ag (assume-decided-statements ag)
                                 (statement-complement (:main-issue ag)))))

(defn filter-policies
  [position]
  (filter #(= (first %) 'valid) position))

(defn- find-position
  [lkifpath acceptability]
  (let [content (import-lkif lkifpath)
        ag (first (:ags content))
        positions (abduction-positions ag acceptability)
        pos (first (sort-by count positions))]
    (prn "pos =")
    (prn pos)
    pos))

(defn find-policies
  [lkifpath acceptability]
  (filter-policies (find-position lkifpath acceptability)))
