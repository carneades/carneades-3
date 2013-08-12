;;; Copyright (c) 2013 Fraunhofer Gesellschaft
;;; Licensed under the EUPL V.1.1

(ns ^{:doc "Upload of documents for a new project"}
  carneades.analysis.web.views.admin.documents.upload
  (:use [jayq.core :only [$ inner attr append]]
        [carneades.analysis.web.i18n :only [i18n]])
  (:require [carneades.analysis.web.views.header :as header]
            [carneades.analysis.web.template :as tp]
            [carneades.analysis.web.dispatch :as dispatch]
            [carneades.analysis.web.views.admin.documents.documents :as documents]))

(defn on-upload-success
  []
  (.fetch (aget js/PM.projects_documents js/PM.project.id) (clj->js {:async false}))
  (js/PM.notify (i18n "upload_successful"))
  (documents/set-url js/PM.project.id))

(dispatch/react-to #{:admin-documents-upload-success} on-upload-success)

(defn on-upload-error
  []
  (js/PM.on_error (i18n "upload_error")))

(defn attach-listeners
  []
  (doto (js/Dropzone. "div#dropzone"
                      (clj->js {:url (str js/IMPACT.wsurl "/documents/" js/PM.project.id)
                                :dictDefaultMessage (i18n "drop_or_click_documents")}))
    (.on "addedfile"
         (fn [] (dispatch/fire :admin-documents-upload-file-added {})))
    (.on "uploadprogress"
         (fn [file progress _]
           (dispatch/fire :admin-documents-upload-progress {:progress progress
                                                           :file file})))
    (.on "success"
         (fn [] (dispatch/fire :admin-documents-upload-success {})))
    (.on "error"
         (fn [] (dispatch/fire :admin-documents-upload-error {})))))

(defn get-url
  [project]
  (str "admin/edit/" project "/documents/upload"))

(defn ^:export show
  [project]
  (js/PM.load_project project)
  (header/show {:text :upload
                :link (str "#/" (get-url project))}
               [])
  (inner ($ ".content")
         (tp/get "admin_documents_upload" {}))
  (attach-listeners))
