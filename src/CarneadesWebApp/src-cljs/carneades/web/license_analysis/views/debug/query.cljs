;;; Copyright (c) 2013 Fraunhofer Gesellschaft
;;; Licensed under the EUPL V.1.1

(ns carneades.web.license-analysis.views.debug.query
  (:use [jayq.core :only [$ inner attr append]]
        [jayq.util :only [log]]
        [carneades.analysis.web.views.core :only [json]])
  (:require [carneades.analysis.web.template :as tp]
            [carneades.analysis.web.views.header :as header]
            [carneades.analysis.web.dispatch :as dispatch]))

(defn display-result
  [msg]
  (.val ($ ".result") (:result msg)))

(dispatch/react-to #{:license-analysis-result-received}
                   (fn [_ msg] (display-result msg)))

(defn display-ask-result
  [msg]
  (.val ($ ".ask-result") (:result msg)))

(dispatch/react-to #{:license-analysis-ask-result-received}
                   (fn [_ msg] (display-ask-result msg)))

(defn on-result-received
  [data]
  (dispatch/fire :license-analysis-result-received
                 (js->clj data :keywordize-keys true)))

(defn on-ask-result-received
  [data]
  (dispatch/fire :license-analysis-ask-result-received
                 (js->clj data :keywordize-keys true)))

(defn send-query
  [msg]
  (log "send query")
  (log "msg=")
  (log (clj->js msg))
  (js/PM.ajax_post (str js/IMPACT.license_analysis_wsurl "/debug/query")
                   (clj->js msg)
                   on-result-received
                   js/IMPACT.user
                   js/IMPACT.password
                   js/PM.on_error))

(dispatch/react-to #{:license-analysis-query-submit}
                   (fn [_ msg]   (send-query msg)))

(defn send-ask
  [msg]
  (log (clj->js msg))
  (js/PM.ajax_post (str js/IMPACT.license_analysis_wsurl "/debug/ask")
                   (clj->js msg)
                   on-ask-result-received
                   js/IMPACT.user
                   js/IMPACT.password
                   js/PM.on_error))

(dispatch/react-to #{:license-analysis-ask-submit}
                   (fn [_ msg]   (send-ask msg)))


(defn on-query-submit
  []
  (let [query (.trim (.val ($ ".sparql-query")))
        limit (js/parseInt (.val ($ ".limit")) 10)
        endpoint (.val ($ ".endpoint"))
        repo-name (.val ($ ".repo-name"))]
    (.val ($ ".result") "Waiting for results...")
    (dispatch/fire :license-analysis-query-submit {:query query
                                                   :limit limit
                                                   :endpoint endpoint
                                                   :repo-name repo-name})))

(defn on-ask-submit
  []
  (let [query (.trim (.val ($ ".sparql-ask")))
        limit (js/parseInt (.val ($ ".limit")) 10)
        endpoint (.val ($ ".endpoint"))
        repo-name (.val ($ ".repo-name"))]
    (.val ($ ".ask-result") "Waiting for results...")
    (dispatch/fire :license-analysis-ask-submit {:query query
                                                 :limit limit
                                                 :endpoint endpoint
                                                 :repo-name repo-name})))

(defn- attach-listeners
  []
  (.click ($ ".submit") on-query-submit)
  (.click ($ ".submit-ask") on-ask-submit))

(defn ^:export show
  [project]
  (header/show {:text :home
                :link "#/home"})
  (inner ($ ".content") (tp/get "license_debug_query"
                                {}))
  (attach-listeners))
