(ns carneades.editor.controller.listeners-register
  (:use clojure.contrib.def
        clojure.contrib.swing-utils
        carneades.editor.view.editorapplication
        carneades.editor.view.tree
        carneades.editor.controller.listeners
        carneades.editor.utils.swing)
  (:import (carneades.editor.uicomponents EditorApplicationView)
           (carneades.editor.view.tree GraphInfo
                                       LkifFileInfo)))

;;
;; For the seperation of concerns, we follow here the MVC pattern,
;; with the controller acting as a mediator between the View and the Model.
;; The View does not have direct access to the Model.
;;
;; http://java.sun.com/developer/technicalArticles/javase/mvc/
;;
;;
;; This namespace directly access the GUI to register Swing listeners and
;; dispatch the calls in an UI-independent way to the listeners in listeners.clj
;;
;; This is the only namespace that should be given direct access to the GUI.
;;
;; All other accesses must be made with the controller/listeners namespace
;; and only through the interface defined by the View.
;;
;; This allow to keep the listeners logic independant from the GUI.
;;

(defvar- *openFileMenuItem* EditorApplicationView/openFileMenuItem)
(defvar- *closeTabMenuItem* EditorApplicationView/closeTabMenuItem)
(defvar- *closeFileMenuItem* EditorApplicationView/closeFileMenuItem)
(defvar- *closeLkifFileMenuItem* EditorApplicationView/closeLkifFileMenuItem)
(defvar- *openGraphMenuItem* EditorApplicationView/openGraphMenuItem)
(defvar- *closeGraphMenuItem* EditorApplicationView/closeGraphMenuItem)

(defvar- *action-listeners*
  [*openFileMenuItem* *closeTabMenuItem*])

(defn- unregister-listeners []
  (doseq [component *action-listeners*]
    (remove-action-listeners component))
  (remove-window-listeners EditorApplicationView/instance)
  (remove-mouse-listeners *lkifsTree*))

(defn- mouse-click-in-tree-listener [event view]
  (let [clickcount (.getClickCount event)]
    (when-let [node (.getLastSelectedPathComponent *lkifsTree*)]
      (when-let [info (.getUserObject node)]
        (prn "info")
        (prn info)
        (case clickcount
            1 (condp typepred info 
                GraphInfo (on-select-graphid view (:path (:lkifinfo info))
                                             (:id info))
                LkifFileInfo (on-select-lkif-file view (:path info))
                nil)
            2 (condp typepred info
                GraphInfo (on-edit-graphid view (:path (:lkifinfo info))
                                           (:id info))
                nil)
            nil)))))

(defn- close-file-listener [event view]
  (prn "close file listener")
  (when-let [node (.getLastSelectedPathComponent *lkifsTree*)]
    (when-let [info (.getUserObject node)]
      (condp typepred info
        LkifFileInfo (on-close-file view (:path info))
        GraphInfo (on-close-file view (:path (:lkifinfo info)))
        nil))))

(defn- close-listener [event view]
  (let [[path id] (current-graph view)]
    (on-close-graph view path id)))

(defn- open-graph-listener [event view]
  (prn "open-graph-listener")
  (when-let [node (.getLastSelectedPathComponent *lkifsTree*)]
    (when-let [info (.getUserObject node)]
      (condp typepred info
        GraphInfo (on-open-graph view (:path (:lkifinfo info)) (:id info))
        nil))))

(defn- close-graph-listener [event view]
  (prn "close-graph-listener")
  (when-let [node (.getLastSelectedPathComponent *lkifsTree*)]
    (when-let [info (.getUserObject node)]
      (condp typepred info
        GraphInfo (on-close-graph view (:path (:lkifinfo info)) (:id info))
        nil))))

(defn register-listeners [view]
  (add-action-listener *openFileMenuItem* (fn [event] (on-open-file view)))
  (add-action-listener *closeFileMenuItem* close-file-listener view)
  (add-action-listener *closeLkifFileMenuItem* close-file-listener view)
  (add-action-listener *openGraphMenuItem* open-graph-listener view)
  (add-action-listener *closeGraphMenuItem* close-graph-listener view)
  (add-action-listener *closeTabMenuItem* close-listener view)
  (add-windowclose-listener
   EditorApplicationView/instance (fn [& args] (unregister-listeners)))
  (add-mousepressed-listener *lkifsTree* mouse-click-in-tree-listener view))
