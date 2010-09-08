;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.editor.controller.listeners
  (:use clojure.contrib.def
        clojure.contrib.pprint
        [clojure.contrib.swing-utils :only (do-swing do-swing-and-wait)]
        carneades.engine.lkif.import
        [carneades.engine.shell :only (search-statements)]
        carneades.engine.argument
        [carneades.engine.statement :only (statement-formatted)]
        carneades.editor.model.docmanager
        ;; only the view.viewprotocol namespace is allowed to be imported
        carneades.editor.view.viewprotocol
        ;; no import of carneades.editor.view.editorapplication,
        ;; java.awt.*, javax.* are not allowed here
        )
  (:require [clojure.string :as str])
  (:import java.io.File))

;;; in this namespace we define the Swing independant listeners

(defvar- *file-error* "File Error")
(defvar- *file-already-opened* "File %s is already opened.")
(defvar- *file-format-not-supported* "This file format is not supported")

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
        (try
          (prn "set busy")
          (set-busy view true)
          (when-let [content (lkif-import path)]
            (add-doc *docmanager* path content)
            (display-lkif-content view file
                                  (map (fn [id] [id (:title (get-ag path id))])
                                       (get-ags-id path)))
            (display-lkif-property view path))
          (finally
           (set-busy view false)))))))

(defn on-select-graphid [view path graphid]
  (let [ag (get-ag path graphid)
        id (:id ag)
        title (:title ag)
        mainissue (statement-formatted (:main-issue ag))]
    (display-graph-property view path title mainissue)))

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

(defvar- *dot-description* "DOT Files")
(defvar- *svg-description* "SVG Files")
(defvar- *graphviz-svg-description* "Graphviz SVG Files")

(defn- suggested-filename [title id ext]
  (if (nil? title)
    (File. (str id "." ext))
    (File. (str (str/join "_" (str/split (str/trim title) #"\s")) "." ext))))

(defn- get-extension [filename]
  (let [idx (.lastIndexOf filename ".")]
    (when (not= idx -1)
     (subs filename (inc idx)))))

(defn on-export-graph [view path id]
  (when-let [ag (get-ag path id)]
    (when-let [[file desc] (ask-file-to-save view {*dot-description* "dot"
                                                   *graphviz-svg-description* "svg"
                                                   *svg-description* "svg"}
                                             (suggested-filename (:title ag) id "svg"))]
      (let [filename (.getPath file)
            extension (get-extension filename)]
        (prn "desc = ")
        (prn desc)
        (condp = desc
            *svg-description*
          (export-graph-to-svg view ag statement-formatted filename)

          *dot-description*
          (export-graph-to-dot view ag statement-formatted filename)

          *graphviz-svg-description*
          (do
            (prn "export graphviz")
            (export-graph-to-graphviz-svg view ag statement-formatted filename))

          ;; for the "All files filter":
          (condp = extension
              "dot"
            (export-graph-to-dot view ag statement-formatted filename)

            "svg"
            (do
              (prn "here")
              (export-graph-to-svg view ag statement-formatted filename))

            (display-error view *file-error* *file-format-not-supported*)))))))

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

(defn on-print-graph [view path id]
  (let [ag (get-ag path id)]
    (print-graph view path ag statement-formatted)))

(defvar- *end-search* (atom false))
(defvar- *nb-agents-running* (atom 0))
(defvar- *running-futures* (atom ()))

(defn- do-one-search [state view]
  (prn "do one search!")
  (let [{:keys [results path id]} state]
    (loop [res results]
      (let [stmt (first res)]
        (when-not (or (nil? stmt) (deref *end-search*))
          (do-swing
           ;; we are not in the swing thread anymore
           ;; so we need to use the do-swing macro
           (display-statement-search-result view path id stmt
                                            statement-formatted))
          (recur (rest res)))))))

(defn wait-for-futures []
  (doseq [future (deref *running-futures*)]
    (prn "waiting for one future...")
    (deref future)))

(defn- do-search [view path id text options]
  (let [{:keys [search-in]} options]
    (when (and (not (empty? text))
               (or (and path (= search-in :current-graph))
                   (= search-in :all-lkif-files)))
      (prn "Search begins")
      (wait-for-futures)
      (prn "Ready to search!")
      (let [path-to-id (if (= search-in :all-lkif-files)
                         (mapcat (fn [path]
                                   (partition 2 (interleave
                                                 (repeat path)
                                                 (get-ags-id path))))
                                 (get-all-keys *docmanager*))
                         [[path id]])
            nb-ids (count path-to-id)]
        (do-swing-and-wait
         (display-search-state view true))
        (reset! *end-search* false)
        (reset! *nb-agents-running* nb-ids)
        (let [searchfutures (doall
                             (map (fn [[path id]]
                                    (let [res (search-statements (get-ag path id)
                                                                 statement-formatted
                                                                 text {})]
                                      (future
                                       (do-one-search {:results
                                                       res
                                                       :path path
                                                       :id id}
                                                      view)))) path-to-id))]
          (reset! *running-futures* searchfutures)
          (future (wait-for-futures)
                  (do-swing-and-wait
                   (display-search-state view false))
                  (prn "setting searching state to false")))))))

(defn on-search-begins [view searchinfo]
  (let [text (first searchinfo)
        options (second searchinfo)
        [path id] (current-graph view)]
    ;; start a separate thread so we can wait for futures to finish
    ;; but not on the swing ui thread (it would cause a deadlock with the
    ;; wait + access from the do-search function)
    (.start (Thread. #(do-search view path id text options)))))

(defn on-search-ends [view]
  (prn "search ends")
  (reset! *end-search* true))

(defn on-select-statement [path id stmt view]
  (prn "on select statement")
  (prn stmt)
  (let [ag (get-ag path id)
        node (get-node ag stmt)
        status (:status node)
        proofstandard (:standard node)
        acceptable (:acceptable node)
        complement-acceptable (:complement-acceptable node)]
    (prn "node = ")
    (prn node)
    (display-statement-property view path (:title ag)
                                (statement-formatted stmt) status
                                proofstandard acceptable complement-acceptable)))

(defn on-select-argument [path id arg view]
  (prn "on select argument")
  (prn arg)
  (display-argument-property
   view path
   (:title (get-ag path id))
   (:title arg)
   (:applicable arg)
   (:weight arg)
   (:direction arg)
   (:scheme arg)))

(defn on-select-premise [path id pm view]
  (prn "on select premise")
  (prn pm)
  (let [type (:type pm)
        typestr (condp = type
                    :carneades.engine.argument/ordinary-premise "Premise"
                    :carneades.engine.argument/assumption "Assumption"
                    :carneades.engine.argument/exception "Exception")]
    (display-premise-property view path (:title (get-ag path id))
                              (:polarity pm) typestr)))

(defn on-select-statement-search [view path id stmt]
  (prn "on select statement search"))

(defn on-open-statement [view path id stmt]
  (prn "on-open-statement")
  (let [ag (get-ag path id)]
    (display-statement view path ag stmt statement-formatted)))
