;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns ^{:doc "Implementation of atomic modifications of data with an history of 
            changes and a dirty marker. This is used to manage undo/redo in the application."}
  carneades.editor.model.history
  (:use clojure.contrib.def))

(defn create-history [content]
  (atom {:index 0 :contents [content] :saved-point 0 :dirty true}))

(defn current-content [history]
  (let [hist (deref history)
        idx (:index hist)
        contents (:contents hist)]
    (get contents idx)))

(defn mark-saved [history]
  (let [{:keys [index]} (deref history)]
    (swap! history assoc :saved-point index :dirty false)))

(defn first-content [history]
  (let [hist (deref history)
        contents (:contents hist)]
    (get contents 0)))

(defn- add-to-history [history content]
  (let [idx (inc (:index history))
        contents (conj (subvec (:contents history) 0 idx) content)]
    (assoc history :index idx :contents contents :dirty true)))

(defn update-content [history content]
  (swap! history add-to-history content))

(defn- can-undo [hist]
  (let [idx (:index hist)]
    (pos? idx)))

(defn can-undo? [history]
  (can-undo (deref history)))

(defn- can-redo [hist]
  (let [idx (:index hist)
        lastidx (dec (count (:contents hist)))]
    (not= idx lastidx)))

(defn can-redo? [history]
  (can-redo (deref history)))

(defn- safe-dec-index [hist]
  (if (can-undo hist)
    (let [idx (:index hist)]
     (assoc hist :index (dec idx) :dirty true))
    hist))

(defn- safe-inc-index [hist]
  (if (can-redo hist)
    (let [idx (:index hist)]
     (assoc hist :index (inc idx) :dirty true))
    hist))

(defn undo [history]
  (swap! history safe-dec-index)
  (current-content history))

(defn redo [history]
  (swap! history safe-inc-index)
  (current-content history))

(defn- delete-hist [hist]
  (let [contents (:contents hist)
        content (get contents (:index hist))]
    (assoc hist :index 0 :contents [content] :dirty true :saved-point nil)))

(defn delete-history
  "delete the history but keeps the current content"
  [history]
  (swap! history delete-hist))

(defn- cancel-upd [hist]
  (let [content (get (:contents hist) 0)]
    (assoc hist :index 0 :contents [content])))

(defn cancel-updates [history]
  (swap! history cancel-upd)
  (current-content history))

(defn restore-to-last-saved
  "restore to last saved point and destroy modification
   since the last saved point"
  [history]
  (let [{:keys [index contents saved-point]} (deref history)
        saved-point (if (nil? saved-point) 0 saved-point)
        contents (subvec contents 0 (inc saved-point))]
    (swap! history assoc :index saved-point :contents contents :dirty false)))

(defn dirty? [history]
  (:dirty (deref history)))