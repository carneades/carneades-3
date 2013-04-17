;;; Copyright (c) 2013 Fraunhofer Gesellschaft
;;; Licensed under the EUPL V.1.1

(ns carneades.policy-analysis.web.views.pmt.report
  (:use [jayq.util :only [log clj->js]]
        [jayq.core :only [$ inner attr]]
        [carneades.policy-analysis.web.views.core :only [template]]
        [carneades.policy-analysis.web.views.pmt.vote :only [round-score]])
  (:require [carneades.policy-analysis.web.backbone.core :as bb])
  (:require-macros [carneades.policy-analysis.web.backbone.macros :as bb]
                   [carneades.policy-analysis.web.views.menu :as menu]))

(bb/defview Report
  :className "pmt-report"
  :render
  ([]
     (js/PM.ajax_get
      (str js/IMPACT.wsurl "/aggregated-poll-results/"
           js/IMPACT.project "/" js/IMPACT.debate_db)
      (fn [results]
        (let [results (js->clj results)
              default-values (reduce (fn [m id] (assoc m id 0.0)) {} (js/PM.get_policies_ids))
              results (merge default-values results)
              policies (reduce (fn [policies [policy_id agreement]]
                                 (let [name (.-title (js/PM.get_policy_header policy_id))]
                                   (cons {:name name
                                          :agreement (round-score (* 100 agreement))
                                          :url (str "#/" (js/PM.policies_url) "/" policy_id)}
                                         policies)))
                                  []
                                  results)]
          (template this :report {:policies (clj->js policies)})
          (js/PM.activate "#report-item")
          (js/PM.attach_lang_listener))))))

(defn ^:export display
  []
  (let [report-view (bb/new
                     Report
                     {:model (bb/new-model
                              {})})]
    (inner ($ "#pm") (.-$el report-view))
    (bb/render report-view)
    false))
