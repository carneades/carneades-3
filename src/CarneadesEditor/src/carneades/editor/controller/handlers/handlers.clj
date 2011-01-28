;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.editor.controller.handlers.handlers
  (:use clojure.contrib.def
        clojure.java.io
        clojure.contrib.trace
        clojure.contrib.pprint
        [clojure.contrib.swing-utils :only (do-swing do-swing-and-wait)]
        carneades.editor.controller.handlers.messages
        (carneades.editor.controller search documents)
        (carneades.engine lkif utils)
        (carneades.engine argument argument-edit)
        [carneades.engine.statement
         :only (statement-formatted statement? str-stmt stmt-str)]
        (carneades.editor.model docmanager properties)
        carneades.editor.utils.core
        ;; only the view.viewprotocol namespace is allowed to be imported
        carneades.editor.view.viewprotocol
        ;; no import of carneades.editor.view.editorapplication,
        ;; java.awt.*, javax.* are not allowed here
        )
  (:require [clojure.string :as str]
            [carneades.editor.model.lkif-utils :as lkif])
  (:import java.io.File))

;;; in this namespace we define the Swing independant listeners

(deftrace create-lkifinfo [path]
  (sort-by second
           (map (fn [id] [id (:title (get-ag path id))])
                (get-ags-id path))))

(deftrace do-close-graph [view path id savechanges]
  "close graph without saving it"
  (prn "do-close-graph")
  (prn "graph before =")
  (let [isfresh (contains? (get-fresh-ag-ids path) id)]
   (when-not savechanges
     (if isfresh
       (remove-section *docmanager* [path :ags id])
       (do
         (update-dirty-state view path (get-ag path id) false)
         (update-undo-redo-statuses view path id)
        (restore-section-to-last-saved *docmanager* [path :ags id]))))
   (close-graph view path id isfresh)))

(deftrace close-all [view path]
  "closes all graphs without saving, removes LKIF from tree"
  (doseq [id (get-ags-id path)]
    (do-close-graph view path id false))
  (hide-lkif-content view path)
  (remove-section *docmanager* [path]))

(deftrace on-select-graphid [view path graphid]
  (let [ag (get-ag path graphid)
        id (:id ag)
        title (:title ag)
        mainissue (statement-formatted (:main-issue ag))]
    ;; (pprint "ag = ")
    ;; (pprint ag)
    ;; (prn)
    (display-graph-property view path id title mainissue)))

(deftrace on-select-lkif-file [view path]
  (prn "on-select-lkif-file")
  (prn "kbs =")
  (let [importurls (get-imports-locations path)]
    (display-lkif-property view path importurls)))

(deftrace on-open-graph [view path id]
  (prn "on-open-graph")
  (when-let [ag (get-ag path id)]
    (open-graph view path ag statement-formatted)
    (when-let [mainissue (:main-issue ag)]
      (display-statement view path ag mainissue statement-formatted))))

(defn do-open-content [view path filename content]
  (init-counters path)
  (let [infos (create-lkifinfo path)]
    (display-lkif-content view path filename infos)
    (when-let [[id _] (first infos)]
      (on-open-graph view path id))))

(defn do-open-file [view path filename rules-directory]
  (if (section-exists? *docmanager* [path])
    (display-error view *file-error* (format *file-already-opened* path))
    (try
      (set-busy view true)
      (when-let [content (import-lkif path rules-directory)]
        (prn "content =")
        (lkif/add-lkif-to-docmanager path content *docmanager*)
        (do-open-content view path filename content))
      (catch IllegalArgumentException
          e (display-error view *open-error* (str *invalid-content* ".")))
      (catch java.io.FileNotFoundException
          e (display-error view *open-error* (str *invalid-content* ": " (.getMessage e))))
      (catch java.io.IOException
          e (display-error view *open-error* (str *invalid-content* ": " (.getMessage e))))
      (catch org.xml.sax.SAXException
          e (display-error view *open-error* *invalid-content*))
      (finally
       (set-busy view false)))))

(deftrace on-open-file [view]
  (prn "ask-lkif-file-to-open...")
  (let [rules-directory (get-property *rules-directory*)]
    (prn "rules-directory =")
    (prn rules-directory)
    (if (empty? rules-directory)
      (display-error view *config-error* *no-rule-directory*)
      (when-let* [file (ask-file-to-open view "LKIF files"  #{"xml" "lkif"})
                  path (.getPath file)
                  filename (.getName file)]
        (do-open-file view path filename rules-directory)))))

(defvar- *dot-description* "DOT Files")
(defvar- *svg-description* "SVG Files")
(defvar- *graphviz-svg-description* "Graphviz SVG Files")

(deftrace suggested-filename [title id ext]
  (if (nil? title)
    (File. (str id "." ext))
    (File. (str (str/join "_" (str/split (str/trim title) #"\s")) "." ext))))

(deftrace get-extension [filename]
  (let [idx (.lastIndexOf filename ".")]
    (when (not= idx -1)
     (subs filename (inc idx)))))

(deftrace on-export-graph [view path id]
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

(deftrace save-lkif [view path]
  "returns false if an error occured, true otherwise"
  (prn "save-lkif")
  (try
    (set-busy view true)
    (let [lkifdata (lkif/extract-lkif-from-docmanager path *docmanager*)]
      (prn "data = ")
      (export-lkif lkifdata path)
      (prn "after save")
      
      true)
    (catch java.io.IOException e
      (display-error view *save-error* (str *error-saving* ": " (.getMessage e)))
      false)
    (finally
     (set-busy view false))))

(deftrace on-saveas [view oldpath]
  (when-let* [[file _] (ask-file-to-save view {"LKIF files (.xml)" "xml"}
                                         (if (new-lkif? oldpath)
                                           (File. (get-newlkif-filename oldpath))
                                           (File. oldpath)))
              path (.getPath file)
              filename (.getName file)]
    (let [content (lkif/extract-lkif-from-docmanager oldpath *docmanager*)
          opened (map second (filter #(= (first %) oldpath) (opened-graphs view)))
          currentid (second (current-graph view))]
      (when (section-exists? *docmanager* [path])
        (close-all view path))
      (lkif/add-lkif-to-docmanager path content *docmanager*)
      (when (save-lkif view path)
        (set-lkif-dirty view path false))
      (do-open-content view path filename content)
      (doseq [id opened]
        (on-open-graph view path id))
      (on-open-graph view path currentid)
      true)))

(deftrace on-save [view path]
  "returns false if an error occured, true otherwise"
  (prn "on-save")
  (if (new-lkif? path)
    (when (on-saveas view path)
      (remove-fresh-ags path)
      (remove-newlkif path)
      (close-all view path))
    (if (save-lkif view path)
      (do
        (remove-fresh-ags path)
        (set-lkif-dirty view path false)
        (mark-lkif-saved view path)
        true)
      false)))

(deftrace on-copy-graph [view path id]
  (when-let [title (read-sentence view *save-as* "Graph title:")]
    (cond
     
     (empty? title)
     (do
       (display-error view *save-as* "Name is empty.")
       (on-copy-graph view path id))
     
     (contains? (get-graphs-titles path) title)
     (do
       (display-error view *save-as* (format "Title '%s' is already used." title))
       (on-copy-graph view path id))

     :else
     (when-let [ag (get-ag path id)]
       (let [newag (assoc ag :title title)
             newid (gen-graph-id path)
             newag (assoc newag :id newid)]
         (add-section *docmanager* [path :ags newid] newag)
         (init-stmt-counter path (:id newag))
         (new-graph-added view path newag statement-formatted)
         (open-graph view path newag statement-formatted)
         (update-dirty-state view path newag true)
         (display-graph-property view path (:id newag) (:title newag)
                                 (statement-formatted (:main-issue newag))))))))

(deftrace on-close-graph [view path id]
  (prn "on-close-graph")
  (if (ag-dirty? path id)
    (case (ask-yesnocancel-question view "Close" "Save file before closing?")
          :yes (when (on-save view path)
                 (do-close-graph view path id true))
          
          :no (do-close-graph view path id false)
          
          :cancel nil)
    (do-close-graph view path id true)))

(deftrace on-export-file [view path]
  (when (ask-confirmation view "Export" "Export all the argument graphs?")
    (doseq [id (get-ags-id path)]
      (on-export-graph view path id))))

(deftrace on-about [view]
  (display-about view))

(deftrace on-printpreview-graph [view path id]
  (let [ag (get-ag path id)]
    (print-preview view path ag statement-formatted)))

(deftrace on-print-graph [view path id]
  (let [ag (get-ag path id)]
    (print-graph view path ag statement-formatted)))

(deftrace on-search-begins [view searchinfo]
  (let [text (first searchinfo)
        options (second searchinfo)
        [path id] (current-graph view)]
    ;; start a separate thread so we can wait for futures to finish
    ;; but not on the swing ui thread (it would cause a deadlock with the
    ;; wait + access from the do-search function)
    (.start (Thread. #(do-search view path id text options)))))

(deftrace on-search-ends [view]
  (prn "search ends")
  (reset! *end-search* true))

(deftrace do-display-statement-property
   [view path id maptitle stmt stmt-fmt status proofstandard acceptable complement-acceptable]
   (display-statement-property
    view path id maptitle stmt stmt-fmt status
    proofstandard acceptable complement-acceptable)
   (set-current-statement-property
    view path id maptitle stmt stmt-fmt status
    proofstandard acceptable complement-acceptable))

(deftrace on-select-statement [path id stmt view]
  (prn "on select statement")
  (when-let* [ag (get-ag path id)
              node (get-node ag stmt)
              status (:status node)
              proofstandard (:standard node)
              acceptable (:acceptable node)
              complement-acceptable (:complement-acceptable node)]
    (do-display-statement-property view path id (:title ag)
                                   (stmt-str stmt) statement-formatted status
                                   proofstandard acceptable complement-acceptable)))

(deftrace on-select-argument [path id arg view]
  (prn "on select argument")
  (when-let* [ag (get-ag path id)
             arg (get-argument ag (:id arg))]
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
     (:scheme arg))))

(deftrace on-select-premise [path id arg pm view]
  (prn "on select premise")
  (prn "arg")
  (prn arg)
  (prn "premise")
  (prn pm)
  (let [type (:type pm)]
    (display-premise-property view path id (:title (get-ag path id))
                              arg
                              (:polarity pm) type (:role pm) (:atom pm))))

(deftrace on-open-statement [view path id stmt]
  (when-let [ag (get-ag path id)]
    (display-statement view path ag stmt statement-formatted)))

(deftrace on-open-argument [view path id arg]
  (when-let [ag (get-ag path id)]
    (display-argument view path ag arg statement-formatted)))

(deftrace do-edit-statement [view path id previous-content-as-obj newcontent oldag]
  (prn "do edit statement")
  (when-let* [ag (update-statement-content oldag previous-content-as-obj newcontent)
              node (get-node ag previous-content-as-obj)
              status (:status node)
              proofstandard (:standard node)
              acceptable (:acceptable node)
              complement-acceptable (:complement-acceptable node)]
    (do-ag-update view [path :ags (:id ag)] ag)
    (do-display-statement-property view path id (:title ag)
                                   (stmt-str newcontent) statement-formatted status
                                   proofstandard acceptable complement-acceptable)
    (statement-content-changed view path ag previous-content-as-obj newcontent)
    (display-statement view path ag newcontent statement-formatted)))

(deftrace on-edit-statement [view path id stmt-info retry-on-error]
  (prn "on-edit-statement")
  (prn stmt-info)
  (let [{:keys [content previous-content]} stmt-info
        oldag (get-ag path id)
        previous-content-as-obj (str-stmt previous-content) 
        newcontent (str-stmt content)
        isnil (nil? newcontent)
        exists (statement-node oldag newcontent)
        ]
    (prn "previous-content-as-obj =")
    (prn previous-content-as-obj)
    (prn "newcontent =")
    (prn newcontent)
    (cond exists
          (display-statement view path oldag newcontent statement-formatted)

          isnil
          (loop [msg *invalid-content*]
            (display-error view *edit-error* msg)
            (when retry-on-error
              (let [content (read-statement view content)
                    newcontent (str-stmt content)]
                (cond (nil? content)
                      nil

                      (nil? newcontent)
                      (recur *invalid-content*)

                      (= newcontent previous-content-as-obj)
                      (display-statement view path oldag newcontent statement-formatted)

                      (statement-node oldag newcontent)
                      (recur *statement-already-exists*)
                      
                      :else
                      (do-edit-statement view path id previous-content-as-obj newcontent oldag)))))
          
          :else (do-edit-statement view path id previous-content-as-obj newcontent oldag))))

(defn on-statement-editor [view path id stmt]
  (prn "on-statement-editor")
  (when-let* [ag (get-ag path id)
              newcontent (read-statement view (stmt-str stmt))
              previous-content (stmt-str stmt)]
    (prn "newcontent =")
    (prn newcontent)
    (prn "previous content =")
    (prn previous-content)
    (on-edit-statement view path id
                       {:previous-content previous-content :content newcontent} true)))

(deftrace on-edit-statement-status [view path id stmt-info]
  (prn "on-edit-statement-status")
  (prn "info = ")
  (prn stmt-info)
  (let [{:keys [status previous-content previous-status]} stmt-info
        oldag (get-ag path id)
        content (str-stmt previous-content)]
    (when (and (not= status previous-status)
               (statement-node oldag content))
      (let [ag (update-statement oldag content status)
            node (get-node ag content)
            status (:status node)
            proofstandard (:standard node)
            acceptable (:acceptable node)
            complement-acceptable (:complement-acceptable node)]
        (do-ag-update view [path :ags (:id oldag)] ag)
        (do-display-statement-property view path id (:title ag)
                                       (str content) statement-formatted status
                                       proofstandard acceptable complement-acceptable)
        (statement-status-changed view path ag content)
        (display-statement view path ag content statement-formatted)))))

(deftrace on-edit-statement-proofstandard [view path id stmt-info]
  (prn "on-edit-statement-proofstandard")
  (prn "stmt-info")
  (prn stmt-info)
  (let [{:keys [proofstandard content previous-proofstandard]} stmt-info
        content (str-stmt content)]
    (when (not= proofstandard previous-proofstandard)
      (when-let* [ag (update-statement-proofstandard (get-ag path id)
                                                     content proofstandard)
                  node (get-node ag content)
                  status (:status node)
                  proofstandard (:standard node)
                  acceptable (:acceptable node)
                  complement-acceptable (:complement-acceptable node)]
        (do-ag-update view [path :ags (:id ag)] ag)
        (do-display-statement-property view path id (:title ag)
                                       (str content) statement-formatted status
                                       proofstandard acceptable complement-acceptable)
        (statement-proofstandard-changed view path ag content)
        (display-statement view path ag content statement-formatted)))))

(deftrace on-undo [view path id]
  (prn "on undo")
  (undo-section *docmanager* [path :ags id])
  (update-undo-redo-statuses view path id)
  (update-dirty-state view path (get-ag path id) true)
  (edit-undone view path id))

(deftrace on-redo [view path id]
  (prn "on redo")
  (redo-section *docmanager* [path :ags id])
  (update-undo-redo-statuses view path id)
  (update-dirty-state view path (get-ag path id) true)
  (edit-redone view path id))

(deftrace on-copyclipboard [view path id]
  (copyselection-clipboard view path id))

(deftrace on-title-edit [view path id ag-info]
  (let [{:keys [previous-title title]} ag-info]
    (when (not= previous-title title)
      (when-let [ag (get-ag path id)]
        (when (ask-confirmation view *rename* *warning-on-rename*)
          (let [ag (assoc ag :title title)]
            (update-section *docmanager* [path :ags id] ag)
            (delete-section-history *docmanager* [path :ags id])
            (update-undo-redo-statuses view path id)
            (title-changed view path ag title)
            (update-dirty-state view path ag true)
            (display-graph-property view path id title
                                    (statement-formatted (:main-issue ag)))))))))

(deftrace on-premise-edit-polarity [view path id pm-info]
  (when-let* [ag (get-ag path id)
              {:keys [atom previous-polarity polarity]} pm-info]
    (when (not= previous-polarity polarity)
      (let [oldarg (:arg pm-info)
            ag (update-premise-polarity ag oldarg atom polarity)
            arg (get-argument ag (:id oldarg))
            title (:title ag)]
        (do-ag-update view [path :ags (:id ag)] ag)
        (premise-polarity-changed view path ag oldarg arg (get-premise arg atom))
        (display-premise-property view path id title
                                  arg
                                  polarity
                                  (:previous-type pm-info)
                                  (:previous-role pm-info) atom)))))

(deftrace on-premise-edit-type [view path id pm-info]
  (prn "on premise edit type")
  (when-let* [ag (get-ag path id)
              {:keys [previous-type type previous-role arg atom]} pm-info]
    (when (not= previous-type type)
      (let [ag (update-premise-type ag arg atom type)
            newarg (get-argument ag (:id arg))
            pm (get-premise newarg atom)]
        (do-ag-update view [path :ags (:id ag)] ag)
        (premise-type-changed view path ag arg newarg (get-premise newarg atom))
        (display-premise-property view path id (:title ag) arg
                                  (:polarity pm) type (:previous-role pm) atom)))))

(deftrace on-premise-edit-role [view path id pm-info]
  (prn "on premise edit role")
  (when-let* [ag (get-ag path id)
              {:keys [previous-role previous-type role arg atom]} pm-info]
    (when (not= previous-role role)
      (let [ag (update-premise-role ag arg atom role)
            newarg (get-argument ag (:id arg))
            pm (get-premise newarg atom)]
        (do-ag-update view [path :ags (:id ag)] ag)
        (premise-role-changed view path ag arg newarg (get-premise newarg atom))
        (display-premise-property view path id (:title ag) arg (:polarity pm)
                                  previous-type role atom)))))

(deftrace on-argument-edit-title [view path id arg-info]
  (prn "on argument edit")
  (when-let* [ag (get-ag path id)
              {:keys [argid previous-title title]} arg-info]
    (when (not= previous-title title)
      (let [arg (get-argument ag argid)
            ag (update-argument-title ag arg title)
            arg (get-argument ag argid)]
        (do-ag-update view [path :ags (:id ag)] ag)
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
        (display-argument view path ag arg statement-formatted)))))

(deftrace on-argument-edit-scheme [view path id arg-info]
  (when-let* [ag (get-ag path id)
              {:keys [argid previous-scheme scheme]} arg-info]
    (when (not= previous-scheme scheme)
      (let [arg (get-argument ag argid)
            ag (update-argument-scheme ag arg scheme)
            arg (get-argument ag argid)]
        (do-ag-update view [path :ags (:id ag)] ag)
        (argument-scheme-changed view path ag arg scheme)
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
        (display-argument view path ag arg statement-formatted))
      )))

(deftrace on-argument-edit-weight [view path id arg-info]
  (prn "on argument edit weight")
  (prn arg-info)
  (when-let* [ag (get-ag path id)
              {:keys [previous-weight weight argid]} arg-info]
    (when (not= previous-weight weight)
      (let [arg (get-argument ag argid)
            newag (update-argument-weight ag arg weight)
            arg (get-argument newag argid)]
        (do-ag-update view [path :ags (:id ag)] newag)
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
        (display-argument view path ag arg statement-formatted)))))

(deftrace on-argument-edit-direction [view path id arg-info]
  (prn "on-argument-edit-direction")
  (prn arg-info)
  (when-let* [ag (get-ag path id)
              {:keys [previous-direction direction argid]} arg-info]
    (when (not= previous-direction direction)
      (let [arg (get-argument ag argid)
            ag (update-argument-direction ag arg direction)
            arg (get-argument ag argid)]
        (do-ag-update view [path :ags (:id ag)] ag)
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
        (display-argument view path ag arg statement-formatted)))))

(deftrace on-add-existing-premise [view path id arg stmt]
  (when-let [ag (get-ag path id)]
    (when (nil? (get-premise arg stmt))
      ;; premise does not already exists!
      (let [arg (get-argument ag (:id arg))]
        (if-let [ag (add-premise ag arg stmt)]
          (let [arg (get-argument ag (:id arg))]
            (do-ag-update view [path :ags (:id ag)] ag)
            (premise-added view path ag arg stmt))
          (display-error view *edit-error* *cycle-error*))))))

(deftrace on-refresh [view path id]
  (when-let [ag (get-ag path id)]
    (do-ag-update view [path :ags (:id ag)] ag)
    (prn "on-refresh")
    (redisplay-graph view path ag statement-formatted)))

(deftrace on-delete-premise [view path id arg pm]
  (prn "pm =")
  (prn pm)
  (when-let* [ag (get-ag path id)
              arg (get-argument ag (:id arg))
              ag (delete-premise ag arg pm)
              arg (get-argument ag (:id arg))]
    (do-ag-update view [path :ags (:id ag)] ag)
    (premise-deleted view path ag arg pm)))

(deftrace prompt-statement-content [view ag suggestion]
  "asks the user the content of a new statement. Prompts
   again if the content is invalid or already exists"
  (let [stmt (read-statement view suggestion)
        stmt-as-obj (str-stmt stmt)]
    (cond (nil? stmt)
          nil

          (nil? stmt-as-obj)
          (do
            (display-error view *edit-error* *invalid-content*)
            (prompt-statement-content view ag stmt))

          (statement-node ag stmt-as-obj)
          (do
            (display-error view *edit-error* *statement-already-exists*)
            (prompt-statement-content view ag stmt))

          :else
          stmt-as-obj)))

(deftrace on-new-premise [view path id arg]
  (prn "on new premise")
  (when-let* [ag (get-ag path id)
              arg (get-argument ag (:id arg))
              stmt (prompt-statement-content view ag "")
              ag (update-statement ag stmt)
              arg (get-argument ag (:id arg))]
    (if-let [ag (add-premise ag arg stmt)]
      (do
        (do-ag-update view [path :ags (:id ag)] ag)
        (new-premise-added view path ag arg stmt statement-formatted)
        (display-statement view path ag stmt statement-formatted))
      (display-error view *edit-error* *cycle-error*))))

(deftrace on-delete-statement [view path id stmt]
  (prn "on delete stmt")
  (prn "stmt =")
  (prn stmt)
  ;; (prn "ag =")
  ;; (pprint (get-ag path id))
  (when-let* [ag (get-ag path id)
              ag (delete-statement ag stmt)]
    (do-ag-update view [path :ags (:id ag)] ag)
    (statement-deleted view path ag stmt)))

(deftrace on-delete-argument [view path id arg]
  (prn "on-delete-argument")
  (when-let* [ag (get-ag path id)
              arg (get-argument ag (:id arg))
              ag (delete-argument ag arg)]
    (do-ag-update view [path :ags (:id ag)] ag)
    (argument-deleted view path ag arg)))

(deftrace on-change-mainissue [view path id stmt]
  (prn "on change mainissue")
  (prn "stmt =")
  (prn stmt)
  (when-let [ag (get-ag path id)]
    (when (or (nil? stmt) (statement-node ag stmt))
      (let [ag (change-mainissue ag stmt)]
        (do-ag-update view [path :ags (:id ag)] ag)
        (mainissue-changed view path ag stmt)))))

(deftrace do-on-new-statement [view path ag stmt]
  (let [ag (update-statement ag stmt)]
    (do-ag-update view [path :ags (:id ag)] ag)
    (new-statement-added view path ag stmt statement-formatted)
    (display-statement view path ag stmt statement-formatted)
    stmt))

(deftrace on-new-statement [view path id]
  "creates a new statements and returns it"
  (when-let [ag (get-ag path id)]
    (let [stmt (gen-statement-content path ag)]
      (when-let [stmt (prompt-statement-content view ag "")]
        (do-on-new-statement view path ag stmt)))))

(deftrace on-new-argument [view path id stmt]
  "creates a new argument and returns it"
  (prn "on new argument")
  (prn "stmt = ")
  (prn stmt)
  (when-let* [ag (get-ag path id)
              arg (pro (gen-argument-id ag) stmt ())
              ag (assert-argument ag arg)]
    (do-ag-update view [path :ags (:id ag)] ag)
    (new-argument-added view path ag arg)
    (display-argument view path ag arg statement-formatted)
    arg))

(deftrace on-new-graph [view path]
  "creates a new graph and returns its id"
  (prn "on new graph")
  (let [title (gen-graph-title path)
        id (gen-graph-id path)
        ag (argument-graph id title nil)
        ag (assoc ag :title title)]
    (add-section *docmanager* [path :ags (:id ag)] ag)
    (init-stmt-counter path (:id ag))
    (add-fresh-ag path (:id ag))
    (new-graph-added view path ag statement-formatted)
    (open-graph view path ag statement-formatted)
    (update-dirty-state view path ag true)
    (display-graph-property view path (:id ag) (:title ag)
                            (statement-formatted (:main-issue ag)))
    id))

(deftrace on-delete-graph [view path id]
  (when (ask-confirmation view "Delete" "Permanently delete the graph?")
    (do-close-graph view path id false)
    (remove-section *docmanager* [path :ags id])
    (set-lkif-dirty view path true)
    (graph-deleted view path id)))

(deftrace create-template [view path]
  (let [id (on-new-graph view path)
        ag (get-ag path id)
        stmt (do-on-new-statement view path ag "Conclusion")
        stmt2 (do-on-new-statement view path ag "Premise")
        arg (on-new-argument view path id stmt)
        ag (get-ag path id)]
    (on-change-mainissue view path id stmt)
    (on-add-existing-premise view path id arg stmt2)
    (on-refresh view path id)
    (delete-section-history *docmanager* [path :ags id])
    (update-undo-redo-statuses view path id)
    (display-statement view path ag stmt statement-formatted)))

(deftrace on-new-file [view]
  (when-let* [file (File/createTempFile "carneades" nil)
              path (.getPath file)
              filename (gen-newlkif-filename path)]
    (.deleteOnExit file)
    (prn "on-new-file, path =")
    (prn path)
    (when (section-exists? *docmanager* [path])
      (close-all view path))
    (lkif/add-lkif-to-docmanager path *empty-lkif* *docmanager*)
    (init-counters path)
    (display-lkif-content view path filename (create-lkifinfo path))
    (create-template view path)))

(deftrace exit [view]
  (letfn [(in-swank?
           []
           (try
             (require 'swank.core)
             true
             (catch Exception e
               false)))]
    (hide view)
    (if (not (in-swank?))
      (do
        (prn "not in swank")
        (System/exit 0))
      (prn "in swank"))))

(deftrace on-exit [view event]
  (prn "on exit")
  (let [unsavedlkifs (get-unsaved-lkifs)]
    (prn "unsavedlkifs =")
    (prn unsavedlkifs)
    (if (empty? unsavedlkifs)
      (exit view)
      (case (ask-yesnocancel-question view "Close" "Save files before closing?")
            :yes (do
                   (doseq [path unsavedlkifs]
                     (on-save view path))
                   (exit view))
            :no (exit view)
            :cancel nil))))

(deftrace on-close-file [view path]
  (prn "on close file")
  (if (lkif-dirty? path)
    (case (ask-yesnocancel-question view "Close" "Save file before closing?")
          :yes (do
                 (on-save view path)
                 (close-all view path))
          :no (close-all view path)
          :cancel nil)
    (close-all view path)))

(deftrace on-import-theory [view path]
  (let [rules-directory (get-property *rules-directory*)]
    (if (empty? rules-directory)
      (display-error view *config-error* *no-rule-directory*)
      (when-let* [info (ask-location-to-open view)
                  {:keys [location relative]} info
                  make-relative (fn [relative pathname root-lkif-dir root-lkif-path rules-directory]
                                  (if-not relative
                                    {:relative-path nil :failed false}
                                    (cond (same-directory? pathname root-lkif-path)
                                          {:relative-path (carneades.engine.utils/make-relative
                                                           pathname root-lkif-dir)
                                           :failed false}

                                          (.startsWith pathname rules-directory)
                                          {:relative-path (carneades.engine.utils/make-relative
                                                           pathname rules-directory)}

                                          :else {:relative-path nil :failed true}
                                          )))
                  root-lkif-dir (.getParent (file path))
                  relative-info (make-relative relative location root-lkif-dir path rules-directory)]
        (prn "location =")
        (prn location)
        (prn "relative-info =")
        (prn relative-info)
        (prn "relative =")
        (prn relative)
        (if (:failed relative-info)
          (do
           (display-error view *import-error*
                          (format *cannot-be-relative* location rules-directory root-lkif-dir))
           (on-import-theory view path))
          (let [lkif (if relative
                       (add-import (get-lkif path)
                                   path
                                   location
                                   (:relative-path relative-info)
                                   rules-directory)
                       (add-import (get-lkif path)
                                   path
                                   location))]
            (update-imports view path lkif)
            (let [importurls (get-imports-locations path)]
              (display-lkif-property view path importurls))))))))

(deftrace on-remove-imports [view path imports]
  (when (and (not (empty? imports)) (ask-confirmation view *imports* *remove-imports*))
    (let [lkif (get-lkif path)
          lkif (reduce (fn [lkif importurl]
                         (let [absolute (as-absolute-import path importurl)]
                           (prn "absolute =")
                           (prn absolute)
                           (remove-import lkif absolute)))
                       lkif imports)]
      (update-imports view path lkif)
      (let [importurls (get-imports-locations path)]
        (display-lkif-property view path importurls)))))

(defn on-edit-preferences [view]
  (prn "on-edit-preferences")
  (when-let* [properties (load-properties)
              newproperties (read-properties view properties)]
    (reset! *properties* newproperties)
    (try
      (store-properties newproperties)
      (catch java.io.IOException e
        (display-error view *save-error* (str *error-saving* ": " (.getMessage e)))
        false))))