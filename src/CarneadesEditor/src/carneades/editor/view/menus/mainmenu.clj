;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.editor.view.menus.mainmenu
  (:use clojure.contrib.def
        clojure.contrib.swing-utils
        carneades.editor.view.components.uicomponents
        carneades.mapcomponent.map)
  (:import carneades.editor.uicomponents.EditorApplicationView))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; public functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn init-menu [])

(defn enable-items [ & items]
  (doseq [item items]
    (.setEnabled item true)))

(defn disable-items [ & items]
  (doseq [item items]
    (.setEnabled item false)))
