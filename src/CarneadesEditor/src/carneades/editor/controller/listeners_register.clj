;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.editor.controller.listeners-register
  (:use clojure.contrib.def
        clojure.contrib.swing-utils
        carneades.editor.controller.swing-listeners
        carneades.editor.controller.listeners
        carneades.editor.utils.swing
        carneades.editor.view.tree
        carneades.editor.view.tabs))

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
;; This is the only namespace, with the swing-listeners,
;; that should be given direct access to the Swing UI.
;;
;; All other accesses must be made within listeners.clj
;; and only through the interface defined by the View.
;;
;; This allow to keep the model and the listeners logic independant
;; from the specific Swing GUI implementation.
;;

(defn register-listeners [view]
  (add-action-listener *openFileButton* (fn [event] (on-open-file view)))
  (add-action-listener *openFileMenuItem* (fn [event] (on-open-file view)))
  (add-action-listener *closeFileMenuItem* close-file-listener view)
  (add-action-listener *exportFileMenuItem* export-file-listener view)
  (add-action-listener *exportLkifFileMenuItem* export-element-listener view)
  (add-action-listener *exportGraphMenuItem* export-element-listener view)
  (add-action-listener *printPreviewFileMenuItem* printpreview-listener view)
  (add-action-listener *aboutHelpMenuItem* (fn [event] (on-about view)))
  (add-action-listener *closeLkifFileMenuItem* close-file-listener view)
  (add-action-listener *openGraphMenuItem* open-graph-listener view)
  (add-action-listener *closeGraphMenuItem* close-graph-listener view)
  (add-action-listener *closeTabMenuItem* close-listener view)
  ;; (add-windowclose-listener
  ;;  *viewinstance* (fn [& args] (unregister-listeners)))
  (add-mousepressed-listener *lkifsTree* mouse-click-in-tree-listener view)

  (register-close-button-listener (fn [event] (close-listener event view))))
