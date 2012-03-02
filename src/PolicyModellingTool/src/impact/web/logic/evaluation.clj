(ns impact.web.logic.evaluation
  (:use impact.web.core
        carneades.database.export
        carneades.engine.argument-graph)
  (:import java.io.File))

(defn show-graph-url
  [pathname]
  (if *debug*
    (str "/map/" (.getName (File. pathname)))
    (str "/argumentbrowser/#/map/" (.getName (File. pathname)))))

(defn get-policies
  [dbname]
  (let [content (export-to-argument-graph dbname)]
    (throw (Exception. "NYI (get-policies)"))
    ;; (map (comp second :statement) (get-nodes (first (:ags content)) 'valid))
    ))

(defn str-to-validstmt
  [s]
  (list 'valid (symbol s)))

(defn evaluate-graph
  [dbname accepted-stmts rejected-stmts]
  (prn "dnname =")
  (prn dbname)
  (throw (Exception. "NYI (evaluate-graph"))
  ;; (let [ag (-> (first (:ags (export-to-argument-graph dbname)))
  ;;              (accept (map str-to-validstmt accepted-stmts))
  ;;              (reject (map str-to-validstmt rejected-stmts)))]
  ;;   (store-ag ag))
  )

(defn- abduction-positions
  [ag acceptability]
  (throw (Exception. "NYI (abduction)"))
  ;; (condp acceptability =
  ;;   :inin (statement-in-label ag (assume-decided-statements ag)
  ;;                             (:main-issue ag))
  ;;   :inout (statement-out-label ag (assume-decided-statements ag)
  ;;                               (:main-issue ag))
  ;;   :outin (statement-in-label ag (assume-decided-statements ag)
  ;;                              (statement-complement (:mainissue ag)))
  ;;   :outout (statement-out-label ag (assume-decided-statements ag)
  ;;                                (statement-complement (:main-issue ag))))
  )

(defn filter-policies
  [position]
  (filter #(= (first %) 'valid) position))

(defn- find-position
  [dbname acceptability]
  (let [content (export-to-argument-graph dbname)
        ag (first (:ags content))
        positions (abduction-positions ag acceptability)
        pos (first (sort-by count positions))]
    (prn "pos =")
    (prn pos)
    pos))

(defn find-policies
  [dbname acceptability]
  (filter-policies (find-position dbname acceptability)))

(defn get-stmt-to-ids
  [dbname]
  (let [content (export-to-argument-graph dbname)
        ag (first (:ags content))]
    (reduce (fn [stmt-to-id stmt]
              (assoc stmt-to-id stmt (:id stmt)))
            {}
            (:statements-nodes ag)
            ;; (map get-statement-node (get-nodes ag))
            )))
