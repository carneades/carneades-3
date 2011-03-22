;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns ^{:doc "Function to display LKIF properties in the panel properties."}
  carneades.editor.view.properties.lkif
  (:use clojure.contrib.def
        clojure.contrib.swing-utils
        carneades.editor.utils.listeners)
  (:import carneades.editor.uicomponents.LkifFilePropertiesView))

(defvar- *lkifProperties* (LkifFilePropertiesView.))
(defvar- *lkifPathText* (.pathTextField *lkifProperties*))
(defvar- *importButton* (.importButton *lkifProperties*))
(defvar- *removeButton* (.removeButton *lkifProperties*))
(defvar- *importsList* (.importsList *lkifProperties*))

(defn init-lkif-properties [])

(defvar- *lkif-info* (atom {}))

(defn get-lkif-properties-panel [path importurls]
  (reset! *lkif-info* {:path path})
  (.setText *lkifPathText* path)
  (.setListData *importsList* (to-array importurls))
  *lkifProperties*)

(defn register-import-button-listener [f args]
  (apply add-action-listener *importButton* f args))

(defn register-remove-import-button-listener [f args]
  (apply add-action-listener *removeButton* f args))

(defn lkif-being-edited-info []
  (merge (deref *lkif-info*)
         {:imports (seq (.getSelectedValues *importsList*))}))