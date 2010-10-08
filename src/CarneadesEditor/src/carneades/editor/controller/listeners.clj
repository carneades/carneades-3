;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.editor.controller.listeners
  (:use clojure.contrib.def
        clojure.contrib.pprint
        [clojure.contrib.swing-utils :only (do-swing do-swing-and-wait)]
        (carneades.editor.controller search documents)
        (carneades.engine.lkif import export)
        (carneades.engine argument argument-edit)
        [carneades.engine.statement :only (statement-formatted statement?)]
        carneades.editor.model.docmanager
        ;; only the view.viewprotocol namespace is allowed to be imported
        carneades.editor.view.viewprotocol
        ;; no import of carneades.editor.view.editorapplication,
        ;; java.awt.*, javax.* are not allowed here
        )
  (:require [clojure.string :as str]
            [carneades.editor.model.lkif-utils :as lkif])
  (:import java.io.File))

;;; in this namespace we define the Swing independant listeners

(defvar- *file-error* "File Error")
(defvar- *edit-error* "Edit Error")
(defvar- *open-error* "Open Error")
(defvar- *save-error* "Save Error")
(defvar- *statement-already-exists* "Statement already exists.")
(defvar- *file-already-opened* "File %s is already opened.")
(defvar- *file-format-not-supported* "This file format is not supported.")
(defvar- *invalid-content* "The content of the file is invalid")
(defvar- *error-saving* "Error while saving")

(defn- create-lkifinfo [path]
  (sort-by second
           (map (fn [id] [id (:title (get-ag path id))])
                (get-ags-id path))))

(defn on-open-file [view]
  (prn "ask-lkif-file-to-open...")
  (when-let [file (ask-lkif-file-to-open view)]
    (let [path (.getPath file)]
      (if (section-exists? *docmanager* [path])
        (display-error view *file-error* (format *file-already-opened* path))
        (try
          (set-busy view true)
          (when-let [content (lkif-import path)]
            (lkif/add-lkif-to-docmanager path content *docmanager*)
            (init-counters path)
            (display-lkif-content view file (create-lkifinfo path))
            (display-lkif-property view path))
          (catch IllegalArgumentException
              e (display-error view *open-error* (str *invalid-content* ".")))
          (catch java.io.IOException
              e (display-error view *open-error* (str *invalid-content* ": " (.getMessage e))))
          (catch org.xml.sax.SAXException
              e (display-error view *open-error* (str *invalid-content* ".")))
          (finally
           (set-busy view false)))))))

(defn on-select-graphid [view path graphid]
  (let [ag (get-ag path graphid)
        id (:id ag)
        title (:title ag)
        mainissue (statement-formatted (:main-issue ag))]
    ;; (pprint "ag = ")
    ;; (pprint ag)
    ;; (prn)
    (display-graph-property view path id title mainissue)))

(defn on-edit-graphid [view path graphid]
  (prn "on-edit-graphid")
  (prn graphid)
  (when-let [ag (get-ag path graphid)]
    (open-graph view path ag statement-formatted)))

(defn on-select-lkif-file [view path]
  (prn "on-select-lkif-file")
  (display-lkif-property view path))

(defn on-close-file [view path]
  (prn "on close file")
  (letfn [(close-all
           [ids]
           (doseq [id ids]
             (close-graph view path id))
           (hide-lkif-content view path)
           (remove-section *docmanager* [path]))]
   (let [ids (get-ags-id path)]
     (if (not (empty? (filter #(is-ag-dirty path %) ids)))
       (when (ask-confirmation view "Close" "Close unsaved file?")
         (close-all ids))
       (close-all ids)))))

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
                                                   ;; *graphviz-svg-description* "svg"
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

(defn- save-lkif [view path]
  "returns false if an error occured, true otherwise"
  (try
    (set-busy view true)
    (let [lkifdata (lkif/extract-lkif-from-docmanager path *docmanager*)]
      (lkif-export lkifdata path)
      true)
    (catch java.io.IOException e
      (display-error view *save-error* (str *error-saving* ": " (.getMessage e)))
      false)
    (finally
     (set-busy view false))))

(defn on-save [view path id]
  "returns false if an error occured, true otherwise"
  (prn "on-save")
  (prn "saving id =")
  (prn id)
  (delete-section-history *docmanager* [path :ags id])
  (update-dirty-state view path (get-ag path id) false)
  (update-undo-redo-statuses view path id)
  (save-lkif view path)
  true
  ;; TODO: fix bug when save-lkif fails
  )

(defn on-close-graph [view path id]
  (prn "on-close-graph")
  (if (is-ag-dirty path id)
    ;; TODO "Save graph before closing it?" [yes] [no] [cancel]
    (case (ask-yesnocancel-question view "Close" "Save graph before closing?")
          :yes (when (on-save view path id)
                 (close-graph view path id))
          
          :no (do
                (cancel-updates-section *docmanager* [path :ags id])
                (update-dirty-state view path (get-ag path id) false)
                (update-undo-redo-statuses view path id)
                (close-graph view path id))
          
          :cancel nil)
    (close-graph view path id)))

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
  (when-let [ag (get-ag path id)]
    (let [node (get-node ag stmt)
          status (:status node)
          proofstandard (:standard node)
          acceptable (:acceptable node)
          complement-acceptable (:complement-acceptable node)]
      (prn node)
      (display-statement-property view path id (:title ag)
                                  (pr-str stmt) statement-formatted status
                                  proofstandard acceptable complement-acceptable))))

(defn on-select-argument [path id arg view]
  (prn "on select argument")
  (when-let [ag (get-ag path id)]
   (let [arg (get-argument ag (:id arg))]
     (prn arg)
     (display-argument-property
      view
      path
      id
      (:title (get-ag path id))
      (:id arg)
      (:title arg)
      (:applicable arg)
      (:weight arg)
      (:direction arg)
      (:scheme arg)))))

(defn on-select-premise [path id arg pm view]
  (prn "on select premise")
  (prn "arg")
  (prn arg)
  (prn "premise")
  (prn pm)
  (let [type (:type pm)]
    (display-premise-property view path id (:title (get-ag path id))
                              arg
                              (:polarity pm) type (:atom pm))))

(defn on-open-statement [view path id stmt]
  (prn "on-open-statement")
  (when-let [ag (get-ag path id)]
    (display-statement view path ag stmt statement-formatted)))

(defn on-open-argument [view path id arg]
  (when-let [ag (get-ag path id)]
    (display-argument view path ag arg statement-formatted)))

(defn- do-update-section [view keys ag]
  "updates section content in the model and dirty markers in the view"
  ;; the first key is the path
  (let [path (first keys)]
    (update-section *docmanager* keys ag)
    (update-undo-redo-statuses view path (:id ag))
    (update-dirty-state view path ag true)))

(defn on-edit-statement [view path id stmt-info]
  (prn "on-edit-statement")
  (prn stmt-info)
  (let [{:keys [content previous-content]} stmt-info
        oldag (get-ag path id)]
    (try
      (let [previous-content-as-obj (read-string previous-content) 
            newcontent (read-string content)]
        (if (not (statement? newcontent))
          (display-error view *edit-error* "Content is invalid.")
          (if (statement-node oldag newcontent)
            (display-error view *edit-error* *statement-already-exists*)
            (when-let [ag (update-statement-content oldag previous-content-as-obj newcontent)]
              (let [stmt (:content stmt-info)
                    node (get-node ag stmt)
                    status (:status node)
                    proofstandard (:standard node)
                    acceptable (:acceptable node)
                    complement-acceptable (:complement-acceptable node)]
                (do-update-section view [path :ags (:id ag)] ag)
                (display-statement-property view path id (:title ag)
                                            (pr-str newcontent) statement-formatted status
                                            proofstandard acceptable complement-acceptable)
                (statement-content-changed view path ag previous-content-as-obj newcontent)
                (display-statement view path ag stmt statement-formatted))))))
      (catch Exception e
        (display-error view *edit-error* "Content is invalid.")))))

(defn on-edit-statement-status [view path id stmt-info]
  (prn "on-edit-statement-status")
  (let [{:keys [status content previous-status]} stmt-info
        oldag (get-ag path id)
        content (read-string content)]
    (when (and (not= status previous-status)
               (statement-node oldag content))
      (let [ag (update-statement oldag content status)
            node (get-node ag content)
            status (:status node)
            proofstandard (:standard node)
            acceptable (:acceptable node)
            complement-acceptable (:complement-acceptable node)]
        (do-update-section view [path :ags (:id oldag)] ag)
        (display-statement-property view path id (:title ag)
                                    (pr-str content) statement-formatted status
                                    proofstandard acceptable complement-acceptable)
        (statement-status-changed view path ag content)
        (display-statement view path ag content statement-formatted)))))

(defn on-edit-statement-proofstandard [view path id stmt-info]
  (prn "on-edit-statement-proofstandard")
  (prn "stmt-info")
  (prn stmt-info)
  (let [{:keys [proofstandard content previous-proofstandard]} stmt-info
        content (read-string content)]
    (when (not= proofstandard previous-proofstandard)
      (when-let [ag (update-statement-proofstandard (get-ag path id)
                                                    content proofstandard)]
        (let [node (get-node ag content)
              status (:status node)
              proofstandard (:standard node)
              acceptable (:acceptable node)
              complement-acceptable (:complement-acceptable node)]
          (do-update-section view [path :ags (:id ag)] ag)
          (display-statement-property view path id (:title ag)
                                      (pr-str content) statement-formatted status
                                      proofstandard acceptable complement-acceptable)
          (statement-proofstandard-changed view path ag content)
          (display-statement view path ag content statement-formatted))))))

(defn on-undo [view path id]
  (prn "on undo")
  (undo-section *docmanager* [path :ags id])
  (update-undo-redo-statuses view path id)
  (update-dirty-state view path (get-ag path id) true)
  (edit-undone view path id))

(defn on-redo [view path id]
  (prn "on redo")
  (redo-section *docmanager* [path :ags id])
  (update-undo-redo-statuses view path id)
  (update-dirty-state view path (get-ag path id) true)
  (edit-redone view path id))

(defn on-saveas [view path id]
  (prn "on save as!"))

(defn on-copyclipboard [view path id]
  (copyselection-clipboard view path id))

(defn on-title-edit [view path id ag-info]
  (let [{:keys [previous-title title]} ag-info]
    (when (not= previous-title title)
      (when-let [ag (get-ag path id)]
        (if (is-ag-dirty path id)
          (display-error view *edit-error* "Please save the graph first.")
          (let [ag (assoc ag :title title)]
            (update-section *docmanager* [path :ags id] ag)
            (delete-section-history *docmanager* [path :ags id])
            (save-lkif view path)
            (display-graph-property view path id title (:main-issue ag))
            (title-changed view path ag title)))))))

(defn on-premise-edit-polarity [view path id pm-info]
  (when-let [ag (get-ag path id)]
    (let [{:keys [atom previous-polarity polarity]} pm-info]
      (when (not= previous-polarity polarity)
        (let [oldarg (:arg pm-info)
              ag (update-premise-polarity ag oldarg atom polarity)
              arg (get-argument ag (:id oldarg))
              title (:title ag)]
          (do-update-section view [path :ags (:id ag)] ag)
          (premise-polarity-changed view path ag oldarg arg (get-premise arg atom))
          (display-premise-property view path id title
                              arg
                              polarity (:previous-type pm-info) atom))))))

(defn on-premise-edit-type [view path id pm-info]
  (when-let [ag (get-ag path id)]
    (let [{:keys [previous-type type arg atom pm]} pm-info]
      (when (not= previous-type type)
        (let [ag (update-premise-type ag arg atom type)
              newarg (get-argument ag (:id arg))]
          (do-update-section view [path :ags (:id ag)] ag)
          (premise-type-changed view path ag arg newarg (get-premise newarg atom))
          (display-premise-property view path id (:title ag) arg (:polarity pm) type atom))))))

(defn on-argument-edit-title [view path id arg-info]
  (prn "on argument edit")
  (prn "info =")
  (prn arg-info)
  (when-let [ag (get-ag path id)]
    (let [{:keys [argid previous-title title]} arg-info]
      (when (not= previous-title title)
        (let [arg (get-argument ag argid)
              ag (update-argument-title ag arg title)
              arg (get-argument ag argid)]
          (do-update-section view [path :ags (:id ag)] ag)
          (argument-title-changed view path ag arg title)
          (display-argument-property
           view
           path
           id
           (:title ag)
           argid
           (:title arg)
           (:applicable arg)
           (:weight arg)
           (:direction arg)
           (:scheme arg))
          (display-argument view path ag arg statement-formatted))))))

(defn on-argument-edit-weight [view path id arg-info]
  (prn "on argument edit weight")
  (prn arg-info)
  (when-let [ag (get-ag path id)]
    (let [{:keys [previous-weight weight argid]} arg-info]
      (when (not= previous-weight weight)
        (let [arg (get-argument ag argid)
              newag (update-argument-weight ag arg weight)
              arg (get-argument newag argid)]
          (do-update-section view [path :ags (:id ag)] newag)
          (argument-weight-changed view path newag arg weight)
          (display-argument-property
           view
           path
           id
           (:title newag)
           argid
           (:title arg)
           (:applicable arg)
           (:weight arg)
           (:direction arg)
           (:scheme arg))
          (display-argument view path ag arg statement-formatted))))))

(defn on-argument-edit-direction [view path id arg-info]
  (prn "on-argument-edit-direction")
  (prn arg-info)
  (when-let [ag (get-ag path id)]
    (let [{:keys [previous-direction direction argid]} arg-info]
      (when (not= previous-direction direction)
        (let [arg (get-argument ag argid)
              ag (update-argument-direction ag arg direction)
              arg (get-argument ag argid)]
          (do-update-section view [path :ags (:id ag)] ag)
          (argument-direction-changed view path ag arg direction)
          (display-argument-property
           view
           path
           id
           (:title ag)
           argid
           (:title arg)
           (:applicable arg)
           (:weight arg)
           (:direction arg)
           (:scheme arg))
          (display-argument view path ag arg statement-formatted))))))

(defn on-add-existing-premise [view path id arg stmt]
  (when-let [ag (get-ag path id)]
    (when (nil? (get-premise arg stmt))
      ;; premise does not already exists!
      (let [arg (get-argument ag (:id arg))
            ag (add-premise ag arg stmt)
            newarg (get-argument ag (:id arg))]
        (do-update-section view [path :ags (:id ag)] ag)
        (premise-added view path ag newarg stmt)))))

(defn on-refresh [view path id]
  (when-let [ag (get-ag path id)]
    (do-update-section view [path :ags (:id ag)] ag)
    (redisplay-graph view path ag statement-formatted)))

(defn on-delete-premise [view path id arg pm]
  (prn "pm =")
  (prn pm)
  (when-let [ag (get-ag path id)]
    (let [arg (get-argument ag (:id arg))
          newag (delete-premise ag arg pm)
          newarg (get-argument ag (:id arg))]
      (do-update-section view [path :ags (:id ag)] newag)
      (prn "ag after delete premise = ")
      (pprint ag)
      (prn)
      (premise-deleted view path newag arg pm))))

(defn on-new-premise [view path id arg]
  (prn "on new premise")
  (when-let [ag (get-ag path id)]
    (let [arg (get-argument ag (:id arg))
          stmt (gen-statement-content path ag)
          ag (update-statement ag stmt)
          arg (get-argument ag (:id arg))
          ag (add-premise ag arg stmt)]
      (do-update-section view [path :ags (:id ag)] ag)
      (new-premise-added view path ag arg stmt statement-formatted)
      (display-statement view path ag stmt statement-formatted)
      )
    )
  )

(defn on-delete-statement [view path id stmt]
  (prn "on delete stmt")
  (prn "stmt =")
  (prn stmt)
  (when-let [ag (get-ag path id)]
    (let [ag (delete-statement ag stmt)]
      (do-update-section view [path :ags (:id ag)] ag)
      (prn "after delete stmt =")
      (pprint ag)
      (prn)
      (statement-deleted view path ag stmt))))

(defn on-delete-argument [view path id arg]
  (prn "on-delete-argument")
  (when-let [ag (get-ag path id)]
    (let [arg (get-argument ag (:id arg))
          ag (delete-argument ag arg)]
      (do-update-section view [path :ags (:id ag)] ag)
      (prn "ag after delete argument = ")
      (pprint ag)
      (prn)
      (argument-deleted view path ag arg))))

(defn on-change-mainissue [view path id stmt]
  (prn "on change mainissue")
  (prn "stmt =")
  (prn stmt)
  (when-let [ag (get-ag path id)]
    (when (or (nil? stmt) (statement-node ag stmt))
      (let [ag (change-mainissue ag stmt)]
        (do-update-section view [path :ags (:id ag)] ag)
        (mainissue-changed view path ag stmt)))))

(defn on-new-statement [view path id]
  (when-let [ag (get-ag path id)]
    (let [stmt (gen-statement-content path ag)
          ag (update-statement ag stmt)]
      (do-update-section view [path :ags (:id ag)] ag)
      (new-statement-added view path ag stmt statement-formatted)
      (display-statement view path ag stmt statement-formatted)
      )
    ))

(defn on-new-argument [view path id stmt]
  (prn "on new argument")
  (prn "stmt = ")
  (prn stmt)
  (when-let [ag (get-ag path id)]
    (let [arg (pro (gen-argument-id ag) stmt ())
          ag (assert-argument ag arg)]
      (do-update-section view [path :ags (:id ag)] ag)
      (new-argument-added view path ag arg)
      (display-argument view path ag arg statement-formatted))))

(defn on-new-graph [view path]
  (prn "on new graph")
  (let [title (gen-graph-title path)
        id (gen-graph-id path)
        ag (argument-graph id title nil)
        ag (assoc ag :title title)]
    (add-section *docmanager* [path :ags (:id ag)] ag)
    (init-stmt-counter path (:id ag))
    (save-lkif view path)
    (new-graph-added view path ag statement-formatted)
    (open-graph view path ag statement-formatted)
    (display-graph-property view path (:id ag) (:title ag) (:main-issue ag))))

(defn on-delete-graph [view path id]
  (when (ask-confirmation view "Delete" "Permanently delete the graph?")
    (close-graph view path id)
    (remove-section *docmanager* [path :ags id])
    (save-lkif view path)
    (graph-deleted view path id)))

(defn on-new-file [view]
  (when-let [[file desc] (ask-file-to-save view {"LKIF files (.xml)" "xml"}
                                           (File. "lkif1.xml"))]
    (let [path (.getPath file)]
      ;; (prn "new file =")
      ;; (prn file)
      ;; (prn "new path =")
      ;; (prn path)
      (lkif/add-lkif-to-docmanager path lkif/*empty-lkif-content* *docmanager*)
      (init-counters path)
      (save-lkif view path)
      (display-lkif-content view file (create-lkifinfo path))
      (display-lkif-property view path)
      )))

(defn- exit []
  (System/exit 0))

(defn on-exit [view event]
  (prn "on exit")
  (let [unsavedgraphs (get-unsaved-graphs)]
    (if (empty? unsavedgraphs)
      (exit)
      (case (ask-yesnocancel-question view "Close" "Save all graphs before closing?")
            :yes (loop [unsavedgraphs unsavedgraphs]
                   (if-let [[path id] (first unsavedgraphs)]
                     (when (on-save view path id)
                       ;; if we save successfully we continue
                       (recur (rest unsavedgraphs)))
                     ;; all saved, exit
                     (exit)))
            :no (exit)
            :cancel nil))))