;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns ^{:doc "The document manager stores a (splitted) LKIF structure and allows
            accesses and modifications to values indexed by a key with transparent
            history and undo / redo support."}
  carneades.editor.model.docmanager
  (:use clojure.contrib.def
        [clojure.contrib.core :only (dissoc-in)]
        carneades.editor.model.history))

(defn create-docmanager []
     (atom {}))

(defn add-section [docmanager keys content]
  "keys is a vector. The first key identifies the document the next 
   the sections or subsections"
  (swap! docmanager assoc-in keys (create-history content)))

(defn update-section [docmanager keys content]
  (let [history (get-in (deref docmanager) keys)]
    (update-content history content)
    content))

(defn remove-section [docmanager keys]
  (swap! docmanager dissoc-in keys))

(defn section-exists? [docmanager keys]
  (not (nil? (get-in (deref docmanager) keys))))

(defn get-section-content [docmanager keys]
  (if-let [history (get-in (deref docmanager) keys)]
    (current-content history)))

(defn get-section-first-content [docmanager keys]
  (if-let [history (get-in (deref docmanager) keys)]
    (first-content history)))

(defn get-all-sectionskeys [docmanager keys]
  (if (empty? keys)
    (clojure.core/keys (deref docmanager))
    (clojure.core/keys (get-in (deref docmanager) keys))))

(defn undo-section [docmanager keys]
  (let [history (get-in (deref docmanager) keys)]
    (undo history)))

(defn cancel-updates-section [docmanager keys]
  (let [history (get-in (deref docmanager) keys)]
    (cancel-updates history)))

(defn redo-section [docmanager keys]
  (let [history (get-in (deref docmanager) keys)]
    (redo history)))

(defn can-undo-section? [docmanager keys]
  (let [history (get-in (deref docmanager) keys)]
    (can-undo? history)))

(defn can-redo-section? [docmanager keys]
  (let [history (get-in (deref docmanager) keys)]
    (can-redo? history)))

(defn delete-section-history [docmanager keys]
  (delete-history  (get-in (deref docmanager) keys)))

(defn mark-section-saved [docmanager keys]
  (mark-saved (get-in (deref docmanager) keys)))

(defn restore-section-to-last-saved [docmanager keys]
  (restore-to-last-saved (get-in (deref docmanager) keys)))

(defn section-dirty? [docmanager keys]
  (dirty? (get-in (deref docmanager) keys)))