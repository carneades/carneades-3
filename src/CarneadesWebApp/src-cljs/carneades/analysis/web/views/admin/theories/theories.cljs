;;; Copyright (c) 2013 Fraunhofer Gesellschaft
;;; Licensed under the EUPL V.1.1

(ns ^{:doc "Displays the theories of a project in the admin page"}
  carneades.analysis.web.views.admin.theories.theories
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
  [theories]
  (.open js/window
         (str js/IMPACT.wsurl
              "/project/"
              js/PM.project.id
              "/theories/"
              theories
              ".clj")))

(dispatch/react-to #{:admin-theories-download} (fn [_ msg] (download (:theories msg))))

(defn on-download-clicked
  [event]
  (.stopPropagation event)
  (when-let [name (selectable-table/selection)]
    (dispatch/fire :admin-theories-download {:theories name}))
  false)

(declare show)

(defn on-delete-successful
  []
  (.fetch (aget js/PM.projects_theories js/PM.project.id) (clj->js {:async false}))
  (show js/PM.project.id))

(defn delete
  [theories]
  (when (js/confirm (i18n "delete_theories_confirmation1"))
    (js/PM.ajax_delete (str js/IMPACT.wsurl "/project/" js/PM.project.id "/theories/" theories)
                       on-delete-successful
                       js/IMPACT.user
                       js/IMPACT.password
                       js/PM.on_error)))

(dispatch/react-to #{:admin-theories-delete} (fn [_ msg] (delete (:theories msg))))

(defn on-delete-clicked
  [event]
  (.stopPropagation event)
  (let [theories (selectable-table/selection)]
    (dispatch/fire :admin-theories-delete {:theories theories}))
  false)

(defn get-url
  [project]
  (str "admin/edit/" project "/theories"))

(defn set-url
  [project]
  (js/jQuery.address.value (get-url project)))

(defn ^:export show
  [project]
  (js/PM.load_project project)
  (header/show {:text :theories
                :link (str "#/" (get-url project))}
               [{:text :upload
                 :link (str "#/admin/edit/" project "/theories/upload")}
                {:text :download
                 :link "#/admin/edit/theories/download"
                 :on on-download-clicked
                 }
                ;; {:text :edit
                ;;  :link "#/admin/edit/theories/edit"}
                {:text :menu_delete
                 :link "#/admin/edit/theories/delete"
                 :on on-delete-clicked}])
  (let [theories (.-theories (json (aget js/PM.projects_theories project)))]
    (inner ($ ".content")
           (tp/get "admin_theories" {:theories theories}))
    (selectable-table/attach ".theories")))
