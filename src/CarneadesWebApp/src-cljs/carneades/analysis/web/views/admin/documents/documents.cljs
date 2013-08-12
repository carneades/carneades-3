;;; Copyright (c) 2013 Fraunhofer Gesellschaft
;;; Licensed under the EUPL V.1.1

(ns ^{:doc "Displays the documents of a project in the admin page"}
  carneades.analysis.web.views.admin.documents.documents
  (:use [jayq.core :only [$ inner attr append]]
        [jayq.util :only [log]]
        [carneades.analysis.web.views.core :only [json]]
        [carneades.analysis.web.i18n :only [i18n]])
  (:require [carneades.analysis.web.views.header :as header]
            [carneades.analysis.web.template :as tp]
            [carneades.analysis.web.dispatch :as dispatch]
            [carneades.analysis.web.views.description-editor :as description]
            [carneades.analysis.web.views.selectable-table :as selectable-table]))

(defn download
  [document]
  (.open js/window
         (str js/IMPACT.wsurl
              "/documents/"
              js/PM.project.id
              "/"
              document)))

(dispatch/react-to #{:admin-documents-download}
                   (fn [_ msg] (download (:document msg))))

(declare show)

(defn on-delete-successful
  []
  (.fetch (aget js/PM.projects_documents js/PM.project.id) (clj->js {:async false}))
  (show js/PM.project.id))

(defn delete
  [document]
  (when (js/confirm (i18n "delete_documents_confirmation1"))
    (js/PM.ajax_delete (str js/IMPACT.wsurl "/documents/" js/PM.project.id "/" document)
                       on-delete-successful
                       js/IMPACT.user
                       js/IMPACT.password
                       js/PM.on_error)))

(dispatch/react-to #{:admin-documents-delete} (fn [_ msg] (delete (:documents msg))))

(defn on-delete-clicked
  [event]
  (.stopPropagation event)
  (let [documents (selectable-table/selection)]
    (dispatch/fire :admin-documents-delete {:documents documents})))

(defn on-download-clicked
  [event]
  (.stopPropagation event)
  (when-let [name (selectable-table/selection)]
    (dispatch/fire :admin-documents-download {:document name})))

(defn get-url
  [project]
  (str "admin/edit/" project "/documents"))

(defn set-url
  [project]
  (js/jQuery.address.value (get-url project)))

(defn ^:export show
  [project]
  (js/PM.load_project project)
  (header/show {:text :documents
                :link (str "#/" (get-url project))}
               [{:text :upload
                 :link (str "#/admin/edit/" project "/documents/upload")}
                {:text :download
                 :link "#/admin/edit/documents/download"
                 :on on-download-clicked
                 }
                {:text :menu_delete
                 :link "#/admin/edit/documents/delete"
                 :on on-delete-clicked}])
  (let [documents (.-documents (json (aget js/PM.projects_documents project)))]
    (inner ($ ".content")
           (tp/get "admin_documents" {:documents documents}))
    (selectable-table/attach ".documents")))
