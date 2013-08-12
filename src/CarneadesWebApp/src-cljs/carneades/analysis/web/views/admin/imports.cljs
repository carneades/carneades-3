;;; Copyright (c) 2013 Fraunhofer Gesellschaft
;;; Licensed under the EUPL V.1.1

(ns ^{:doc "Displays the projects import page"}
  ;; there is a bug if naming this ns 'import',
  ;; ClojureScript generates a import$ object
  carneades.analysis.web.views.admin.imports
  (:use [jayq.core :only [$ inner attr append]]
        [jayq.util :only [log]]
        [carneades.analysis.web.views.core :only [json]]
        [carneades.analysis.web.i18n :only [i18n]])
  (:require [carneades.analysis.web.views.header :as header]
            [carneades.analysis.web.template :as tp]
            [carneades.analysis.web.dispatch :as dispatch]
            [carneades.analysis.web.views.admin.project :as project]))

(defn upload-progress
  [file progress]
  (if (= progress 100)
    (js/PM.notify (i18n "server_processing"))
    (js/PM.notify (str "File:" (.-name file) " " progress (i18n "progress_message")))))

(dispatch/react-to #{:import-upload-progress}
                   (fn [_ msg]
                     (upload-progress (:file msg) (:progress msg))))

(defn on-upload-success
  []
  (.fetch js/PM.projects (clj->js {:async false}))
  (js/PM.notify (i18n "import_successful"))
  (project/set-url))

(dispatch/react-to #{:import-success} on-upload-success)

(defn on-upload-error
  []
  (js/PM.on_error (i18n "import_error")))

(dispatch/react-to #{:import-error} on-upload-error)

(defn attach-listeners
  []
  (doto (js/Dropzone. "div#dropzone"
                      (clj->js {:url (str js/IMPACT.wsurl "/import")
                                :dictDefaultMessage (i18n "drop_or_click")}))
    (.on "addedfile"
         (fn [] (dispatch/fire :import-file-added {})))
    (.on "uploadprogress"
         (fn [file progress _]
           (dispatch/fire :import-upload-progress {:progress progress
                                                   :file file})))
    (.on "success"
         (fn [] (dispatch/fire :import-success {})))
    (.on "error"
         (fn [] (dispatch/fire :import-error {})))))

(defn ^:export show
  []
  (header/show {:text :admin
                :link "#/admin/project"}
               [])
  (inner ($ ".content")
         (tp/get "admin_import" {}))
  (attach-listeners))
