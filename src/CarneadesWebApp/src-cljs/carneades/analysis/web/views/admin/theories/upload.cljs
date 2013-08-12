;;; Copyright (c) 2013 Fraunhofer Gesellschaft
;;; Licensed under the EUPL V.1.1

(ns ^{:doc "Upload of theories for a new project"}
  carneades.analysis.web.views.admin.theories.upload
  (:use [jayq.core :only [$ inner attr append]]
        [carneades.analysis.web.i18n :only [i18n]])
  (:require [carneades.analysis.web.views.header :as header]
            [carneades.analysis.web.template :as tp]
            [carneades.analysis.web.dispatch :as dispatch]
            [carneades.analysis.web.views.admin.theories.theories :as theories]))

(defn upload-progress
  [file progress]
  (if (= progress 100)
    (js/PM.notify (i18n "server_processing"))
    (js/PM.notify (str "File:" (.-name file) " " progress (i18n "progress_message")))))

;; (dispatch/react-to #{:admin-theories-upload-progress}
;;                    (fn [_ msg]
;;                      (upload-progress (:file msg) (:progress msg))))

(defn on-upload-success
  []
  (.fetch (aget js/PM.projects_theories js/PM.project.id) (clj->js {:async false}))
  (js/PM.notify (i18n "upload_successful"))
  (theories/set-url js/PM.project.id))

(dispatch/react-to #{:admin-theories-upload-success} on-upload-success)

(defn on-upload-error
  []
  (js/PM.on_error (i18n "upload_error")))


(defn attach-listeners
  []
  (doto (js/Dropzone. "div#dropzone"
                      (clj->js {:url (str js/IMPACT.wsurl "/project/" js/PM.project.id "/theories")
                                :dictDefaultMessage (i18n "drop_or_click_theories")}))
    (.on "addedfile"
         (fn [] (dispatch/fire :admin-theories-upload-file-added {})))
    (.on "uploadprogress"
         (fn [file progress _]
           (dispatch/fire :admin-theories-upload-progress {:progress progress
                                                           :file file})))
    (.on "success"
         (fn [] (dispatch/fire :admin-theories-upload-success {})))
    (.on "error"
         (fn [] (dispatch/fire :admin-theories-upload-error {})))))

(defn get-url
  [project]
  (str "admin/edit/" project "/theories/upload"))

(defn ^:export show
  [project]
  (js/PM.load_project project)
  (header/show {:text :upload
                :link (str "#/" (get-url project))}
               [])
  (inner ($ ".content")
         (tp/get "admin_theories_upload" {}))
  (attach-listeners))
