;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.editor.controller.listeners
  (:use clojure.contrib.def
        clojure.contrib.pprint
        carneades.engine.lkif.import
        [carneades.engine.statement :only (statement-formatted)]
        carneades.editor.model.docmanager
        carneades.editor.view.editorapplication
        ;; no import of java.awt.* or javax.* are allowed here
        )
  (:import java.io.File))

;;; in this namespace we define the Swing independant listeners

(defvar- *file-error* "File Error")
(defvar- *file-already-opened* "File %s is already opened.")

(defvar- *docmanager* (create-docmanager))

(defn- get-ag [lkifpath id]
  (first (filter #(= (:id %) id)
                 (:ags (get-doc-content *docmanager* lkifpath)))))

(defn- get-ags-id [lkifpath]
  (map :id (:ags (get-doc-content *docmanager* lkifpath))))

(defn on-open-file [view]
  ;; (display-graph view tompkins statement-formatted)
  (prn "ask-lkif-file-to-open...")
  (when-let [file (ask-lkif-file-to-open view)]
    (let [path (.getPath file)]
      (if (doc-exists? *docmanager* path)
        (display-error view *file-error* (format *file-already-opened* path))
        (when-let [content (lkif-import path)]
          (add-doc *docmanager* path content)
          (display-lkif-content view file (get-ags-id path))
          (display-lkif-property view path))))))

(defn on-select-graphid [view path graphid]
  (let [ag (get-ag path graphid)
        id (:id ag)
        title (:title ag)
        mainissue (statement-formatted (:main-issue ag))]
    (display-graph-property view id title mainissue)))

(defn on-edit-graphid [view path graphid]
  (prn "on-edit-graphid")
  (prn graphid)
  (when-let [ag (get-ag path graphid)]
    (open-graph view path ag statement-formatted)))

(defn on-close-graph [view path id]
  (prn "on-close-graph")
  (close-graph view path id))

(defn on-select-lkif-file [view path]
  (prn "on-select-lkif-file")
  (display-lkif-property view path))

(defn on-close-file [view path]
  (prn "on close file")
  (doseq [id (get-ags-id path)]
    (close-graph view path id))
  (hide-lkif-content view path)
  (remove-doc *docmanager* path))

(defn on-open-graph [view path id]
  (open-graph view path (get-ag path id) statement-formatted))

(defn on-export-graph [view path id]
  (when-let [ag (get-ag path id)]
    (when-let [file (ask-file-to-save view "SVG Files" "svg"
                                      (File. (str id ".svg")))]
      (let [filename (.getPath file)]
        (export-graph-to-svg view ag statement-formatted filename)))))

(defn on-close-graph [view path id]
  (close-graph view path id))

(defn on-export-file [view path]
  (when (ask-confirmation view "Export" "Export all the argument graphs?")
    (doseq [id (get-ags-id path)]
      (on-export-graph view path id))))

(defn on-about [view]
  (display-about view))

(defn on-printpreview-graph [view path id]
  (let [ag (get-ag path id)]
    (print-preview view path ag statement-formatted)))
