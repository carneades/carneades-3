;; Copyright (c) 2012 Fraunhofer Gesellschaft
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns carneades.maps.subset-ag
  (:use clojure.pprint
        carneades.engine.statement
        carneades.engine.argument
        carneades.engine.argument-graph
        carneades.engine.caes
        carneades.engine.argument))

(defn is-root?
  [ag stmt]
  (empty? (:premise-of (get-statement-node ag stmt))))

(defn has-root?
  [ag arg]
  (is-root? ag (:conclusion arg)))

(defn add-arg
  [subset arg]
  (enter-argument subset (map->argument arg)))

(defn expand-border
  [ag border]
  (set (mapcat (fn [arg]
                 (mapcat (fn [premise]
                           (map (fn [argid] (get-argument-node ag argid))
                                (concat (:pro (get-statement-node ag (literal-atom premise)))
                                        (:con (get-statement-node ag (literal-atom premise))))))
                         (:premises arg)))
               border)))

(defn walk-ag
  [subset ag border visited depth treeify]
  (if (or (zero? depth) (empty? border))
    subset
    (let [subset (reduce add-arg subset border)
          newborder (expand-border ag border)]
      (recur subset ag newborder (apply conj visited border) (dec depth) treeify))))

(defn update-statuses
  [subset ag]
  ;; TODO
  subset
  ;; (let [statements (vals (:statement-nodes subset))]
  ;;   (reduce (fn [subset stmt]
  ;;             (cond (accepted? ag stmt) (accept subset [stmt])
  ;;                   (rejected? ag stmt) (reject subset [stmt])
  ;;                   (questioned? ag stmt) (question subset [stmt])
  ;;                   :else subset))
  ;;           subset
  ;;           statements))
  )

(defn subset-ag
  [ag & options]
  ag
  ;; (let [options (apply hash-map options)
  ;;       {:keys [depth treeify] :or {treeify true depth Integer/MAX_VALUE}} options
  ;;       argroots (filter (fn [arg] (has-root? ag arg)) (arguments ag))
  ;;       subset (walk-ag (make-argument-graph) ag argroots #{} depth treeify)
  ;;       subset (update-statuses subset ag)]
  ;;   subset)
  )
