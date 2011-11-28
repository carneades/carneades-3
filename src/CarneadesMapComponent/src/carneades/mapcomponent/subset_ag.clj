(ns carneades.mapcomponent.subset-ag
  (:use clojure.pprint
        carneades.engine.statement
        carneades.engine.argument))

(defn is-root?
  [ag stmt]
  (empty? (:premise-of (statement-node ag stmt))))

(defn has-root?
  [ag arg]
  (is-root? ag (:conclusion arg)))

(defn add-arg
  [subset arg]
  (assert-argument subset arg))

(defn expand-border
  [ag border]
  (set (mapcat (fn [arg]
                 (mapcat (fn [premise]
                           (map (fn [argid] (get-argument ag argid))
                                (:conclusion-of (get-node ag (:atom premise)))))
                         (:premises arg)))
               border)))

(defn walk-ag
  [subset ag border visited depth treeify]
  (if (or (zero? depth) (empty? border))
    subset
    (let [subset (reduce add-arg subset border)
          newborder (expand-border ag border)]
      (recur subset ag newborder (apply conj visited border) (dec depth) treeify))))

(defn subset-ag
  [ag & options]
  (let [options (apply hash-map options)
        {:keys [depth treeify] :or {treeify true depth Integer/MAX_VALUE}} options
        argroots (filter (fn [arg] (has-root? ag arg)) (arguments ag))
        subset (walk-ag (argument-graph) ag argroots #{} depth treeify)]
    subset))
