;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.editor.controller.listeners-register
  (:use clojure.contrib.def
        clojure.contrib.swing-utils
        carneades.editor.view.swinguiprotocol
        carneades.editor.controller.swing-listeners
        carneades.editor.controller.listeners
        carneades.editor.utils.swing
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
;; that should be given direct access to the Swing UI and only through
;; the SwingUI protocol.
;;
;; All other accesses must be made within listeners.clj
;; and only through the View protocol.
;;
;; This allow to keep the model and the listeners logic independant
;; from the specific Swing GUI implementation.
;;

(defn register-listeners [view]
  ;; we need to extract some information from the UI,
  ;; dispatch to the swing_listeners:
  (add-close-graph-menuitem-listener view close-graph-listener [view])
  (add-open-graph-menuitem-listener view open-graph-listener [view])
  (add-close-lkif-filemenuitem-listener view close-file-listener [view])
  (add-printpreview-filemenuitem-listener view printpreview-listener [view])
  (add-print-filemenuitem-listener view print-listener [view])
  (add-export-graph-menuitem-listener view export-element-listener [view])
  (add-export-lkif-filemenuitem-listener view export-element-listener [view])
  (add-export-filemenuitem-listener view export-file-listener [view])
  (add-close-file-menuitem-listener view close-file-listener [view])
  (add-close-button-listener view close-listener [view])
  (add-mousepressed-tree-listener view mouse-click-in-tree-listener [view])

  ;; we don't need to extract information from the UI,
  ;; dispatch to the listeners:
  (add-about-helpmenuitem-listener view (fn [event] (on-about view)) [])
  (add-open-file-menuitem-listener view (fn [event] (on-open-file view)) [])
  (add-open-file-button-listener view (fn [event] (on-open-file view)) []))
