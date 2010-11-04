
(ns carneades.engine.lkif
  ;(:require )
  (:use
    clojure.contrib.def
    carneades.engine.lkif.import
    carneades.engine.lkif.export
    carneades.engine.owl
    carneades.engine.rule
    )
  ;(:import )
  )

(def lkif-import lkif-import*)
(def lkif-export lkif-export*)

; TODO
(defn- get-imported-ont
  ([imp-tree imp-kbs]
    (some (fn [i] (let [kb (get imp-kbs (:name i))]
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

(defn generate-arguments-from-lkif
  ([lkif]
    (generate-arguments-from-lkif lkif :reasoner nil nil nil))
  ([lkif type]
    (generate-arguments-from-lkif lkif type nil nil nil))
  ([lkif type ont]
    (generate-arguments-from-lkif lkif type nil nil ont))
  ([lkif type cq opt ont]
    (let [imp-kbs (:import-kbs lkif)
          rb (:rb lkif)]
      (fn [subgoal state]
        (concat
          (let [d-ont (or ont (get-imported-ont (:import-tree lkif) imp-kbs))]
            ;(println "domain ontology:" d-ont)
            ((generate-arguments-from-rules rb cq d-ont) subgoal state)) ; direct rules
          (apply concat
            (map                                                      ; imported kbs
              (fn [kbe]
                (let [name (key kbe),
                      kb (val kbe)]
                  (if (contains? kb :ontology)
                    ;owl
                    ((generate-arguments-from-owl kb type) subgoal state)
                    ;rb
                    (let [d-ont (or ont (get-imported-ont (:import-tree lkif) imp-kbs name))] ; todo : maybe other way around
                      ;(println "domain ontology:" d-ont)
                      ((generate-arguments-from-rules kb cq d-ont) subgoal state)))))
              imp-kbs)))))))


