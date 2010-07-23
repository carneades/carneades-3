(ns carneades.editor.model.docmanager
  (:use clojure.contrib.def))

(defn create-docmanager []
     (atom {}))

(defn add-doc [docmanager path content]
  (swap! docmanager assoc path content))

(defn remove-doc [docmanager path]
  (swap! docmanager dissoc path))

(defn doc-exists? [docmanager path]
  (contains? (deref docmanager) path))

(defn get-doc-content [docmanager path]
  (get (deref docmanager) path))
