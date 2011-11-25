;;; Copyright (c) 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns ^{:doc "Import and export of argument graphs and rules 
            from/to the LKIF format"}
  carneades.xml.lkif.lkif
  (:use clojure.contrib.def
        clojure.pprint
        clojure.java.io
        carneades.engine.lkif.export
        carneades.engine.owl
        carneades.engine.rule
        carneades.engine.argument-builtins
        carneades.engine.utils)
  (:require carneades.engine.lkif.import))

(def ^{:doc "Import from a LKIF. See the documentation in the import namespace"}
  import-lkif carneades.engine.lkif.import/import-lkif)
;; (def import-lkif /import-lkif)
(def ^{:doc "Export to LKIF. See the documentation in the export namespace"}
  export-lkif lkif-export*)

; TODO
(defn- get-imported-ont
  ([imp-tree imp-kbs]
    ;(println "get-imported ont")
    ;(println "imp-tree" imp-tree)
    ;(println "imp-kbs" imp-kbs)
    (some (fn [i] (let [kb (get imp-kbs (:name i))]
                    ;(println "checking kb" (:name i) kb)
                    (and (contains? kb :ontology) kb)))
      imp-tree)
    )
  ([imp-tree imp-kbs rb-name]
    (let [i (some (fn [i] (and (= (:name i) rb-name) i)) imp-tree)]
      (if i
        (some (fn [i2] (let [kb (get imp-kbs (:name i2))]
                         (and (contains? kb :ontology) kb)))
          (:import-tree i))
        (some (fn [i2] (get-imported-ont i2 imp-kbs rb-name)) (map :import-tree imp-tree))))))

(defn add-import-helper
  ([lkif i-path relative-path resolve-path]
     (if (carneades.engine.lkif.import/lkif? i-path)
       (let [i (carneades.engine.lkif.import/import-lkif-helper i-path resolve-path ())
             new-i-tree (cons {:name i-path
                               :relative-path relative-path
                               :import-tree (:import-tree i)}
                              (:import-tree lkif))
             irb (:rb i)
             i-kbs (if irb
                     (assoc (:import-kbs i) i-path irb)
                     (:import-kbs i))
             new-i-kbs (merge (:import-kbs lkif) i-kbs)
             iags (:ags i)
             i-ags (if iags
                     (assoc (:import-ags i) i-path iags)
                     (:import-ags i))
             new-i-ags (merge (:import-ags lkif) i-ags)]
         (assoc lkif :import-tree new-i-tree :import-kbs new-i-kbs :import-ags new-i-ags))
       (let [o (load-ontology i-path)
             new-i-tree (cons {:name i-path
                               :relative-path relative-path
                               :import-tree nil}
                              (:import-tree lkif))
             new-i-kbs (assoc (:import-kbs lkif) i-path o)]
         (assoc lkif :import-tree new-i-tree :import-kbs new-i-kbs)))))

(defn add-import
  "see import-lkif"
  ([lkif root-lkif-path pathname]
     (let [root-lkif-dir (parent pathname)]
       (add-import-helper lkif pathname nil
                          (fn [pathname parent-pathname]
                            (carneades.engine.lkif.import/local-resolve pathname parent-pathname
                                                                        root-lkif-dir)))))
  ([lkif root-lkif-path pathname relative-pathname basedir]
     (let [root-lkif-dir (parent pathname)]
      (add-import-helper lkif pathname relative-pathname
                         (fn [pathname parent-pathname]
                           (carneades.engine.lkif.import/local-then-base-dir-resolve
                            pathname parent-pathname root-lkif-dir basedir))))))

(defn- flatten-import-tree
  [i-tree]
  (apply concat (map (fn [i] (cons (:name i) (flatten-import-tree (:import-tree i)))) i-tree)))

(defn- occurs-in?
  [i-tree i-name]
  (let [imp-names (flatten-import-tree i-tree)]
    ;(println "occurs in?" imp-names i-name)
    (some #{i-name} imp-names)))

(defn remove-import
  [lkif i-path]
  (let [r-path (filter #(= (:name %) i-path) (:import-tree lkif))
        ;; _ (do (prn "r-path =") (prn r-path) true)
        new-i-tree (filter (fn [e] (not (= (:name e) i-path))) (:import-tree lkif)),
        ;; _ (do (prn "new-i-tree =") (pprint new-i-tree) true)
        rec-imps (cons i-path (flatten-import-tree r-path))
        ;; _ (do (prn "rec-imps =") (pprint rec-imps) true)
        unused-imps (filter (fn [i] (not (occurs-in? new-i-tree i))) rec-imps),
        ;; _ (do (prn "unused-imps =") (pprint unused-imps) true)
        new-i-kbs (reduce dissoc (:import-kbs lkif) unused-imps)
        ;; _ (do (prn "new-i-kbs =") (pprint new-i-kbs) true)
        new-i-ags (reduce dissoc (:import-ags lkif) unused-imps)
        ;; _ (do (prn "new-i-ags =") (pprint new-i-ags) true)
        ]
    ;(println "recursive imports" rec-imps)
    ;(println "new import tree" new-i-tree)
    ;(println "unused imports" unused-imps)
    (assoc lkif :import-tree new-i-tree :import-kbs new-i-kbs :import-ags new-i-ags)))

(defn generate-arguments-from-lkif
  ([lkif]
    (generate-arguments-from-lkif lkif :reasoner nil nil nil))
  ([lkif type]
    (generate-arguments-from-lkif lkif type nil nil nil))
  ([lkif type ont]
    (generate-arguments-from-lkif lkif type nil nil ont))
  ([lkif type opt ont]
    (let [imp-kbs (:import-kbs lkif),
          rb (:rb lkif)]
      (fn [subgoal state]
        (concat
          (let [d-ont (or ont (get-imported-ont (:import-tree lkif) imp-kbs))]
            ;(println "domain ontology:" d-ont)
            ;(println "generating from rules" rb)
            ((builtins (list (generate-arguments-from-owl d-ont :reasoner))) subgoal state)) ; builtins
          (let [d-ont (or ont (get-imported-ont (:import-tree lkif) imp-kbs))]
            ;(println "domain ontology:" d-ont)
            ;(println "generating from rules" rb)
            ((generate-arguments-from-rules rb d-ont) subgoal state)) ; direct rules
          (apply concat
            (map                                                      ; imported kbs
              (fn [kbe]
                (let [name (key kbe),
                      kb (val kbe)]
                  (if (contains? kb :ontology)
                    ;owl
                    (do
                      ;(println "generating from ontology" name)
                      ((generate-arguments-from-owl kb type) subgoal state))
                    ;rb
                    (let [d-ont (or ont (get-imported-ont (:import-tree lkif) imp-kbs name) (get-imported-ont (:import-tree lkif) imp-kbs))] ; todo : maybe other way around
                      ;(println "domain ontology:" d-ont)
                      ;(println "generating from rules" name)
                      ((generate-arguments-from-rules kb d-ont) subgoal state)))))
              imp-kbs)))))))


(def *empty-lkif*
  {:sources nil, :rb *empty-rulebase*, :ags '(), :import-tree '(), :import-kbs {}, :import-ags {}})