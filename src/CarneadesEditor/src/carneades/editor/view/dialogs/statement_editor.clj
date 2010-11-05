;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.editor.view.dialogs.statement-editor
  (:use clojure.contrib.def
        clojure.contrib.swing-utils
        carneades.editor.view.components.uicomponents)
  (:import (carneades.editor.uicomponents EditStatementDialog)))

(defvar *statement-being-edited-editor-info* (atom nil))

(defvar- *statement-editor-listeners* (atom ()))

(defn register-statement-editor-listener [f args]
  (swap! *statement-editor-listeners* conj {:listener f :args args}))

(defn show-statement-editor
  ([content]
     (show-statement-editor content true))
  ([content call-listener]
     (let [dialog (EditStatementDialog. *frame* true content)
           okbutton (.okbutton dialog)
           cancelbutton (.cancelbutton dialog)
           contenttext (.statementContentText dialog)]
       (reset! *statement-being-edited-editor-info* {:previous-content content})
       (.setText contenttext content)
       (add-action-listener cancelbutton
                            (fn [event]
                              (.dispose dialog)
                              (swap! *statement-being-edited-editor-info*
                                     assoc :content nil)))
       (add-action-listener okbutton
                            (fn [event]
                              (swap! *statement-being-edited-editor-info*
                                     assoc :content (.getText contenttext))
                              (.dispose dialog)
                              (when call-listener
                                (doseq [{:keys [listener args]}
                                        (deref *statement-editor-listeners*)]
                                  (apply listener event args)))))
       (.setLocationRelativeTo dialog *frame*)
       (.setVisible dialog true)
       (:content (deref *statement-being-edited-editor-info*)))))

