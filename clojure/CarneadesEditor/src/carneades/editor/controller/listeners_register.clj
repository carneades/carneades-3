(ns carneades.editor.controller.listeners-register
  (:use clojure.contrib.def
        clojure.contrib.swing-utils
        carneades.editor.view.editorapplication
        carneades.editor.controller.listeners)
  (:import (carneades.editor.uicomponents EditorApplicationView)
           javax.swing.event.ListSelectionListener))

;;
;; For the seperation of concerns, we follow here the MVC pattern,
;; with the controller acting as a mediator between the View and the Model.
;; The View does not have direct access to the Model.
;;
;; http://java.sun.com/developer/technicalArticles/javase/mvc/
;;
;;
;; This namespace directly access the GUI to register listeners.
;; This is the only namespace that should be given direct access to it.
;;
;; All other accesses must be made with the controller/listeners namespace
;; and only though the interface defined by the View.
;;
;; This allow to keep the listeners logic independant from the GUI.
;;

(defn add-listselection-listener
  [component f & args]
  (let [listener (proxy [ListSelectionListener] []
                   (valueChanged [event] (apply f event args)))]
    (.addListSelectionListener component listener)
    listener))

(defn register-listeners [view]
  (add-action-listener
   EditorApplicationView/fileOpenFileMenuItem on-open-file view)
  (add-listselection-listener
   EditorApplicationView/argumentgraphsList on-select-graph-id view))

