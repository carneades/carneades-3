;;; Copyright (c) 2013 Fraunhofer Gesellschaft
;;; Licensed under the EUPL V.1.1

(ns ^{:doc "Displays the projects on the admin page"}
  carneades.analysis.web.views.admin.project
  (:use [jayq.core :only [$ inner attr append]]
        [jayq.util :only [log]]
        [carneades.analysis.web.views.core :only [json]]
        [carneades.analysis.web.i18n :only [i18n]])
   (:require [carneades.analysis.web.views.header :as header]
             [carneades.analysis.web.template :as tp]
             [carneades.analysis.web.dispatch :as dispatch]
             [carneades.analysis.web.views.admin.properties :as properties]
             [carneades.analysis.web.views.selectable-table :as selectable-table]))

(defn export
  [project]
  (.open js/window (str js/IMPACT.wsurl "/export/" project ".zip")))

(dispatch/react-to #{:admin-export} (fn [_ msg]  (log "msg=" msg) (export (:project msg))))

(defn delete-successful
  []
  (.fetch js/PM.projects (clj->js {:async false})))

(declare show)

(defn delete
  [project]
  (when (and (js/confirm (i18n "delete_confirmation1"))
             (js/confirm (i18n "delete_confirmation2")))
    (.destroy (.get js/PM.projects project)
              (clj->js {:success show}))))

(dispatch/react-to #{:admin-delete} (fn [_ msg] (delete (:project msg))))

(defn edit
  [project]
  (properties/set-url project))

(dispatch/react-to #{:admin-edit} (fn [_ msg] (edit (:project msg))))

(defn on-export-clicked
  [event]
  (.stopPropagation event)
  (when-let [project (selectable-table/selection)]
    (dispatch/fire :admin-export {:project project}))
  false)

(defn on-delete-clicked
  [event]
  (.stopPropagation event)
  (when-let [project (selectable-table/selection)]
    (dispatch/fire :admin-delete {:project project}))
  false)

(defn on-edit-clicked
  [event]
  (.stopPropagation event)
  (when-let [project (selectable-table/selection)]
    (dispatch/fire :admin-edit {:project project}))
  false)

(defn get-url
  []
  (str "admin/project"))

(defn set-url
  []
  (js/jQuery.address.value (get-url)))


(defn ^:export show
  []
  (header/show {:text :admin
                :link "#/admin/project"}
               [{:text :menu_import
                 :link "#/admin/import"}
                {:text :menu_export
                 :link "#/admin/export"
                 :on on-export-clicked}
                {:text :edit
                 :link "#/admin/edit/"
                 :on on-edit-clicked}
                {:text :menu_delete
                 :link "#/admin/delete"
                 :on on-delete-clicked}])
  (inner ($ ".content")
         (tp/get "admin_project"
                 {:projects (json js/PM.projects)}))
  (selectable-table/attach ".projects"))
