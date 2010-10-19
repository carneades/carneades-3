;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.editor.view.application.editorapplication
  (:use carneades.editor.utils.type
        (carneades.editor.view viewprotocol swinguiprotocol)
        (carneades.editor.view.application editorview editorswingui))
  (:import carneades.editor.view.application.editorview.EditorView
           carneades.editor.view.application.editorswingui.EditorSwingUI))

(defrecord SwingView [view swingui])

(auto-extend SwingView carneades.editor.view.viewprotocol/View (:view this))
(auto-extend SwingView carneades.editor.view.swinguiprotocol/SwingUI (:swingui this))

(defn create-swingview []
  (let [view (EditorView.)
        swingui (EditorSwingUI.)]
    (SwingView. view swingui)))
