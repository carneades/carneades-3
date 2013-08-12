;;; Copyright (c) 2013 Fraunhofer Gesellschaft
;;; Licensed under the EUPL V.1.1

(ns carneades.policy-analysis.web.views.pmt.submitted-facts
  (:use [jayq.util :only [log]]
        [jayq.core :only [$ inner attr append]]
        [carneades.analysis.web.views.core :only [template]]
        [cljs.reader :only [read-string]]
        [carneades.policy-analysis.web.views.pmt.questions :only [show-facts]])
  (:require [carneades.analysis.web.backbone.core :as bb]
            [carneades.analysis.web.template :as template])
  (:require-macros ;; [carneades.analysis.web.backbone.macros :as bb]
                   [carneades.policy-analysis.web.views.menu :as menu]))

(defn show-facts-for-modification
  "Shows the facts of the ag to the user for modification."
  [questions]
  (inner ($ "#pm") (template/get "submitted_facts" {}))
  (show-facts questions {:wsurl js/IMPACT.simulation_url}))

(defn ^:export display
  "Displays the facts submitted by the user for verification and correction."
  []
  (js/PM.ajax_post js/IMPACT.simulation_url
                   (clj->js {:modifiable-facts
                             {:policies
                              (.get js/PM.project "policies")
                              :project js/IMPACT.project
                              :db js/IMPACT.db}})
                   show-facts-for-modification
                   js/IMPACT.user
                   js/IMPACT.password
                   js/PM.on_error)
  false)
