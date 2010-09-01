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
        ;; java.awt.*, javax.* are allowed here
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
          (display-lkif-content view file
                                (map (fn [id] [id (:title (get-ag path id))])
                                     (get-ags-id path)))
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

(defn on-print-graph [view path id]
  (let [ag (get-ag path id)]
    (print-graph view path ag statement-formatted)))

(defvar- *end-search* (atom false))

(defn on-search-begins [view searchinfo]
  (prn "Beginning search")
  (let [text (first searchinfo)
        options (second searchinfo)
        [path id] (current-graph view)]
    (if path
      (let [ag (get-ag path id)
            searchagent (agent {:results
                                (search-statements ag statement-formatted
                                                   {:search-content text})})
            searchfn (fn [state]
                       (try
                         (let [{:keys [results]} state]
                           (loop [res results]
                             (let [stmt (first res)]
                               (prn "stmt = ")
                               (prn stmt)
                               (prn "end search =")
                               (prn (deref *end-search*))
                               (when-not (or (nil? stmt) (deref *end-search*))
                                 (do-swing
                                  ;; we are not in the swing thread anymore
                                  ;; so we need to use the do-swing macro
                                  (display-statement-search-result view path id stmt
                                                                   statement-formatted))
                                 (recur (rest res))))))
                         (finally (do-swing
                                   (display-search-state view false)))))]
        (display-search-state view true)
        (reset! *end-search* false)
        (send searchagent searchfn))
      (display-search-state view false))))

(defn on-search-ends [view]
  (prn "Stopping search")
  (reset! *end-search* true))

(defn on-select-statement [path id stmt view]
  (prn "on select statement")
  (prn stmt)
  (let [node (get-node (get-ag path id) stmt)
        status (:status node)
        proofstandard (:standard node)
        acceptable (:acceptable node)
        complement-acceptable (:complement-acceptable node)]
    (prn "node = ")
    (prn node)
    (display-statement-property view (statement-formatted stmt) status
                                proofstandard acceptable complement-acceptable)))

(defn on-select-argument [path id arg view]
  (prn "on select argument")
  (prn arg)
  (display-argument-property
   view (:title arg) (:applicable arg) (:weight arg) (:direction arg) (:scheme arg)))

(defn on-select-premise [path id pm view]
  (prn "on select premise")
  (prn pm)
  (let [type (:type pm)
        typestr (condp = type
                    :carneades.engine.argument/ordinary-premise "Premise"
                    :carneades.engine.argument/assumption "Assumption"
                    :carneades.engine.argument/exception "Exception")]
    (display-premise-property view (:polarity pm) typestr)))

(defn on-select-statement-search [view path id stmt]
  (prn "on select statement search"))

(defn on-open-statement [view path id stmt]
  (prn "on-open-statement")
  (let [ag (get-ag path id)]
    (display-statement view path ag stmt statement-formatted)))
