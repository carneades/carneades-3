;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.editor.model.docmanager
  (:use clojure.contrib.def))

(defn create-docmanager []
     (atom {}))

(defn add-doc [docmanager key content]
  (swap! docmanager assoc key content))

(defn remove-doc [docmanager key]
  (swap! docmanager dissoc key))

(defn doc-exists? [docmanager key]
  (contains? (deref docmanager) key))

(defn get-doc-content [docmanager key]
  (get (deref docmanager) key))

(defn count-docs [docmanager]
  (count (keys (deref docmanager))))

(defn get-all-keys [docmanager]
  (keys (deref docmanager)))

