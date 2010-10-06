;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.editor.controller.swing-listeners
  (:use clojure.contrib.def
        clojure.contrib.swing-utils
        carneades.editor.view.viewprotocol
        carneades.editor.view.swinguiprotocol
        carneades.editor.controller.listeners
        carneades.editor.utils.swing)
  (:import (carneades.editor.uicomponents EditorApplicationView)
           (carneades.editor.view.swinguiprotocol GraphInfo
                                                  LkifFileInfo
                                                  StatementInfo
                                                  ArgumentInfo)))

(defn mouse-click-in-tree-listener [event view]
  (let [clickcount (.getClickCount event)]
    (when-let [info (get-selected-object-in-tree view)]
      (case clickcount
            1 (condp instance? info 
                GraphInfo (on-select-graphid view (:path (:lkifinfo info))
                                             (:id info))
                LkifFileInfo (on-select-lkif-file view (:path info))
                nil)
            2 (condp instance? info
                GraphInfo (on-edit-graphid view (:path (:lkifinfo info))
                                           (:id info))
                nil)
            nil))))

(defn mouse-click-in-searchresult [event view]
  (let [clickcount (.getClickCount event)]
    (when-let [info (get-selected-object-in-search-result view)]
      (when (= clickcount 2)
        (condp instance? info
          StatementInfo (on-open-statement view (:path info) (:id info)
                                           (:stmt info))

          ArgumentInfo (on-open-argument view (:path info) (:id info) (:arg info))
                nil)))))

(defn keyenter-in-searchresult [event view]
  (when-let [info (get-selected-object-in-search-result view)]
    (condp instance? info
      StatementInfo (on-open-statement view (:path info) (:id info)
                                       (:stmt info))

      ArgumentInfo (on-open-argument view (:path info) (:id info) (:arg info))
      nil)))

(defn close-file-listener [event view]
  (prn "close file listener")
  (when-let [info (get-selected-object-in-tree view)]
    (condp instance? info
      LkifFileInfo (on-close-file view (:path info))
      GraphInfo (on-close-file view (:path (:lkifinfo info)))
      nil)))

(defn close-button-listener [event view]
  (prn "close-button-listener")
  (let [[path id] (get-graphinfo-being-closed view event)]
    (on-close-graph view path id)))

(defn open-graph-listener [event view]
  (prn "open-graph-listener")
  (when-let [info (get-selected-object-in-tree view)]
    (condp instance? info
      GraphInfo (on-open-graph view (:path (:lkifinfo info)) (:id info))
      nil)))

(defn close-graph-listener [event view]
  (prn "close-graph-listener")
  (when-let [info (get-selected-object-in-tree view)]
    (condp instance? info
      GraphInfo (on-close-graph view (:path (:lkifinfo info)) (:id info))
      nil)))

(defn export-file-listener [event view]
  (when-let [info (get-selected-object-in-tree view)]
    (condp instance? info
      GraphInfo (if-let [[path id] (current-graph view)]
                  (on-export-graph view path id)
                  (on-export-graph view (:path (:lkifinfo info)) (:id info)))
      LkifFileInfo (if-let [[path id] (current-graph view)]
                     (on-export-graph view path id)
                     (on-export-file view (:path info)))
      nil)))

(defn export-element-listener [event view]
  (when-let [info (get-selected-object-in-tree view)]
    (condp instance? info
      GraphInfo (on-export-graph view (:path (:lkifinfo info)) (:id info))
      LkifFileInfo (on-export-file view (:path info))
      nil)))

(defn printpreview-listener [event view]
  (if-let [[path id] (current-graph view)]
    (on-printpreview-graph view path id)))

(defn print-listener [event view]
  (if-let [[path id] (current-graph view)]
    (on-print-graph view path id)))

(defn search-result-selection-listener [event view]
  (let [info (get-selected-object-in-search-result view)]
    (condp instance? info
      StatementInfo (on-select-statement (:path info) (:id info)
                                         (:stmt info) view)

      ArgumentInfo (on-select-argument (:path info) (:id info) (:arg info) view)
      nil)))

(defn statement-button-edit-listener [event view]
  (let [info (get-statement-being-edited-info view)
        {:keys [path id]} info]
    (on-edit-statement view path id info)))

(defn statement-status-edit-listener [event view]
  (prn "statement-status-edit-listener")
  (when (.isFocusOwner (.getSource event))
    ;; comboBox fires twice the action event...
    (let [info (get-statement-being-edited-info view)
          {:keys [path id previous-content]} info]
      ;; we don't take in account the current content of the TextArea when just
      ;; editing the status
      (on-edit-statement-status view path id (assoc info :content previous-content)))))

(defn statement-proofstandard-edit-listener [event view]
  (when (.isFocusOwner (.getSource event))
    ;; comboBox fires twice the action event...
    (let [info (get-statement-being-edited-info view)
          {:keys [path id previous-content]} info]
      (on-edit-statement-proofstandard view path id (assoc info :content previous-content)))))

(defn undo-button-listener [event view]
  (when-let [[path id] (current-graph view)]
    (on-undo view path id)))

(defn redo-button-listener [event view]
  (when-let [[path id] (current-graph view)]
    (on-redo view path id)))

(defn refresh-button-listener [event view]
  (when-let [[path id] (current-graph view)]
    (on-refresh view path id)))

(defn undo-editmenuitem-listener [event view]
  (undo-button-listener event view))

(defn redo-editmenuitem-listener [event view]
  (redo-button-listener event view))

(defn save-button-listener [event view]
  (when-let [[path id] (current-graph view)]
    (on-save view path id)))

(defn copyclipboard-button-listener [event view]
  (when-let [[path id] (current-graph view)]
    (on-copyclipboard view path id)))

(defn save-filemenuitem-listener [event view]
  (save-button-listener event view))

(defn saveas-filemenuitem-listener [event view]
  (when-let [[path id] (current-graph view)]
    (on-saveas view path id)))

(defn title-edit-listener [event view]
  (let [info (get-graph-being-edited-info view)]
    (on-title-edit view (:path info) (:id info) info)))

(defn premise-edit-polarity-listener [event view]
  (let [info (get-premise-being-edited-info view)]
    (on-premise-edit-polarity view (:path info) (:id info) info)))

(defn premise-edit-type-listener [event view]
  (let [info (get-premise-being-edited-info view)]
    (on-premise-edit-type view (:path info) (:id info) info)))

(defn argument-edit-title-listener [event view]
  (let [info (get-argument-being-edited-info view)]
    (on-argument-edit-title view (:path info) (:id info) info)))

(defn argument-edit-weight-listener [event view]
  (let [info (get-argument-being-edited-info view)]
    (on-argument-edit-weight view (:path info) (:id info) info)))

(defn argument-edit-direction-listener [event view]
  (let [info (get-argument-being-edited-info view)]
    (on-argument-edit-direction view (:path info) (:id info) info)))

(defn delete-premise-menuitem-listener [event view]
  (when-let [[path id] (current-graph view)]
    (let [{:keys [pm arg]} (get-selected-node view path id)]
      (on-delete-premise view path id arg pm))))

(defn new-premise-menuitem-listener [event view]
  (when-let [[path id] (current-graph view)]
    (let [arg (get-selected-node view path id)]
      (on-new-premise view path id arg))))

(defn delete-statement-menuitem-listener [event view]
  (when-let [[path id] (current-graph view)]
    (let [stmt (get-selected-node view path id)]
      (on-delete-statement view path id stmt))))

(defn delete-argument-menuitem-listener [event view]
  (when-let [[path id] (current-graph view)]
    (let [arg (get-selected-node view path id)]
      (on-delete-argument view path id arg))))

(defn mainissue-menuitem-listener [event view]
  (when-let [[path id] (current-graph view)]
    (let [stmt (get-selected-node view path id)
          selected (.isSelected (.getSource event))]
      (if selected
       (on-change-mainissue view path id stmt)
       (on-change-mainissue view path id nil)))))

(defn new-statement-menuitem-listener [event view]
  (when-let [[path id] (current-graph view)]
    (on-new-statement view path id)))
