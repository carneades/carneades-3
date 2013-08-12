;;; Copyright (c) 2013 Fraunhofer Gesellschaft
;;; Licensed under the EUPL V.1.1

(ns carneades.web.license-analysis.views.debug.introduction
  (:use [jayq.util :only [log]]
        [jayq.core :only [$ inner attr append]]
        [carneades.analysis.web.views.core :only [json]])
  (:require [carneades.analysis.web.template :as tp]
            [carneades.analysis.web.views.header :as header]
            [carneades.analysis.web.dispatch :as dispatch]
            [carneades.web.license-analysis.views.debug.facts :as facts]))

(defn start-dialog
  [msg]
  (js/PM.load_project (:project msg))
  (js/PM.ajax_post (str js/IMPACT.license_analysis_wsurl "/debug/analyse")
                   (clj->js msg)
                   (fn [data]
                     (js/PM.busy_cursor_off)
                     (log "start-dialog, received question")
                     (log data)
                     (facts/show data))
                   js/IMPACT.user
                   js/IMPACT.password
                   js/PM.on_error))

(dispatch/react-to #{:license-analysis-start-dialog}
                   (fn [_ msg] (start-dialog msg)))

(defn on-reload-projects-successfull
  []
  (.fetch js/PM.projects (clj->js {:async false}))
  (js/PM.notify "Reload successful"))

(defn reload-projects
  [msg]
  (js/PM.ajax_get (str js/IMPACT.wsurl "/debug/reload-projects")
                  on-reload-projects-successfull
                  js/PM.on_error))

(dispatch/react-to #{:license-analysis-reload-projects}
                   (fn [_ msg] (reload-projects msg)))

(defn on-reload-projects
  []
  (dispatch/fire :license-analysis-reload-projects {}))

(defn on-start-dialog
  []
  (let [project (.val ($ ".project"))
        query (.val ($ ".query"))
        theories (.val ($ ".theories"))
        repo-name (.val ($ ".repo-name"))
        ag-name (.val ($ ".ag-name"))
        endpoint (.val ($ ".endpoint"))]
    (dispatch/fire :license-analysis-start-dialog {:query query
                                                   :project project
                                                   :entity ""
                                                   :theories theories
                                                   :endpoint endpoint
                                                   :repo-name repo-name
                                                   :ag-name ag-name})))

(defn attach-listeners
  []
  (.click ($ ".reload-projects") on-reload-projects)
  (.click ($ ".start-dialog") on-start-dialog))

(defn ^:export show
  []
  (header/show {:text :home
                :link "#/home"})
  (inner ($ ".content") (tp/get "license_debug_introduction" {}))
  (attach-listeners))
