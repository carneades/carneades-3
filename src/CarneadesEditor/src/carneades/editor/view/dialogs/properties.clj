;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.editor.view.dialogs.properties
  (:use clojure.contrib.def
        clojure.contrib.swing-utils
        carneades.editor.view.viewprotocol
        carneades.editor.view.components.uicomponents)
  (:import (carneades.editor.uicomponents.propertiesdialog PropertiesDialog
                                                           FilePropertyPanel
                                                           StringPropertyPanel)
           (javax.swing JFileChooser)))

(defmulti make-panel (fn [_ _ _ _ type _] type))

(defmethod make-panel :default [view propertiespanel value name _ newproperties]
  (let [panel (FilePropertyPanel.)
        propertyLabel (.propertyLabel panel)
        fileText (.fileText panel)]
    (.setText propertyLabel name)
    (.setText fileText value)
    (.add propertiespanel panel)
    {:get-user-value (fn [] (.getText fileText))}))

(defmethod make-panel :file [view propertiespanel value name _ newproperties]
  (let [panel (FilePropertyPanel.)
        propertyLabel (.propertyLabel panel)
        fileText (.fileText panel)
        browseButton (.browseButton panel)]
    (add-action-listener browseButton
                         (fn [_]
                           (when-let [file (ask-file-to-open
                                            view "LKIF files"  #{"xml" "lkif"})]
                             (.setText fileText (str (.getPath file))))))
    (.setText propertyLabel name)
    (.setText fileText value)
    (.add propertiespanel panel)
    {:get-user-value (fn [] (.getText fileText))}))

(defmethod make-panel :directory [view propertiespanel value name _ newproperties]
  (let [panel (FilePropertyPanel.)
        propertyLabel (.propertyLabel panel)
        fileText (.fileText panel)
        browseButton (.browseButton panel)]
    (add-action-listener browseButton
                         (fn [_]
                           (let [jc (JFileChooser.)]
                             (.setFileSelectionMode jc JFileChooser/DIRECTORIES_ONLY)
                             (let [val (.showOpenDialog jc *frame*)]
                               (when (= val JFileChooser/APPROVE_OPTION)
                                 (.setText fileText
                                           (str
                                            (.getPath (.getSelectedFile jc)))))))))
    (.setText propertyLabel name)
    (.setText fileText value)
    (.add propertiespanel panel)
    {:get-user-value (fn [] (.getText fileText))}))

(defn show-properties-dialog [view properties]
  (let [dialog (PropertiesDialog. *frame* true)
        propertiespanel (.propertiesPanel dialog)
        okbutton (.okButton dialog)
        cancelbutton (.cancelButton dialog)
        newproperties (atom nil)
        panels (reduce (fn [panels property]
                             (let [[k {:keys [value name type]}] property]
                               (assoc panels k (make-panel view
                                                           propertiespanel
                                                           value
                                                           name
                                                           type
                                                           newproperties))))
                       {} properties)]
    (add-action-listener okbutton
                         (fn [_]
                           (let [props (reduce (fn [props panel]
                                                 (let [[k v] panel
                                                       {:keys [get-user-value]} v]
                                                   (update-in props [k :value]
                                                              (fn [_] (get-user-value)))))
                                               properties panels)]
                             (.dispose dialog)
                             (reset! newproperties props))))
    (add-action-listener cancelbutton (fn [_]
                                        (.dispose dialog)
                                        (reset! newproperties nil)))
    (.setLocationRelativeTo dialog *frame*)
    (.setVisible dialog true)
    (deref newproperties)))