;;; Copyright (c) 2013 Fraunhofer Gesellschaft
;;; Licensed under the EUPL V.1.1

(ns catb.views.pmt.submitted-facts
  (:use [jayq.util :only [log clj->js]]
        [jayq.core :only [$ inner attr append]]
        [catb.views.core :only [template]]
        [cljs.reader :only [read-string]]
        [catb.views.pmt.questions :only [show-facts]])
  (:require [catb.backbone.core :as bb]
            [catb.template :as template])
  (:require-macros [catb.backbone.macros :as bb]
                   [catb.views.menu :as menu]))

(defn show-facts-for-modification
  "Shows the facts of the ag to the user for modification."
  [questions]
  (inner ($ "#pm") (template/get "submitted_facts" {}))
  (show-facts questions))

(defn ^:export display-facts
  "Displays the facts submitted by the user for verification and correction."
  []
  (js/PM.ajax_post js/IMPACT.simulation_url
                   (clj->js {:modifiable-facts {:theory js/IMPACT.current_policy
                                                :db js/IMPACT.db}})
                   show-facts-for-modification
                   js/IMPACT.user
                   js/IMPACT.password
                   js/PM.on_error)
  false)
