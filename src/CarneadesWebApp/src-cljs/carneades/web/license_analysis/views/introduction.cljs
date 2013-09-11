(ns carneades.web.license-analysis.views.introduction
  (:require [jayq.core :refer [$ inner attr append]]
            [jayq.util :refer [log]]
            [carneades.analysis.web.template :as tp]
            [carneades.analysis.web.views.header :as header]
            [carneades.analysis.web.dispatch :as dispatch]
            [carneades.web.license-analysis.views.debug.facts :as facts]))

;; http://<markos-server-domain>/markos/#/license-analysis/introduction?entity=<project-uri>

(defn start-facts-gathering
  [msg]
  (js/PM.ajax_post (str js/IMPACT.license_analysis_wsurl "/analyse")
                   (clj->js msg)
                   (fn [data]
                     (js/PM.busy_cursor_off)
                     (log "start-facts-gathering, received question")
                     (log data)
                     (facts/show data))
                   js/IMPACT.user
                   js/IMPACT.password
                   js/PM.on_error))

(dispatch/react-to #{:license-analysis-start-facts-gathering}
                   (fn [_ msg] (start-facts-gathering msg)))

(defn on-start-analysis
  []
  (let [entity (.parameter js/jQuery.address "entity")]
    (log "entity=" entity)
    (dispatch/fire :license-analysis-start-facts-gathering {:entity entity}))
  false)

(defn attach-listeners
  []
  (.click ($ "#start") on-start-analysis))

(defn ^:export show
  [project]
  (js/PM.load_project project)
  (header/show {:text :home
                :link "#/home"})
  (inner ($ ".content") (tp/get "license_introduction" {}))
  (attach-listeners))
