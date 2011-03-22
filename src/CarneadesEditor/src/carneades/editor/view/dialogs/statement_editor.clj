;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.editor.view.dialogs.statement-editor
  (:use clojure.contrib.def
        clojure.contrib.swing-utils
        carneades.editor.view.components.uicomponents
        carneades.editor.utils.listeners)
  (:import (carneades.editor.uicomponents EditStatementDialog)))

;; move to _helper?
(defvar *statement-being-edited-menu-info* (atom nil))

(defn show-statement-editor
  [content]
  (let [dialog (EditStatementDialog. *frame* true content)
        okbutton (.okbutton dialog)
        cancelbutton (.cancelbutton dialog)
        contenttext (.statementContentText dialog)]
    (swap! *statement-being-edited-menu-info* assoc :previous-content content)
    (.setText contenttext content)
    (add-action-listener cancelbutton
                         (fn [event]
                           (.dispose dialog)
                           (swap! *statement-being-edited-menu-info*
                                  assoc :content nil)))
    (add-action-listener okbutton
                         (fn [event]
                           (swap! *statement-being-edited-menu-info*
                                  assoc :content (.getText contenttext))
                           (.dispose dialog)))
    (.setLocationRelativeTo dialog *frame*)
    (.setVisible dialog true)
    (:content (deref *statement-being-edited-menu-info*))))

