;;; Copyright (c) 2012 Fraunhofer Gesellschaft
;;; Licensed under the EUPL V.1.1

(ns catb.views.pmt.facts
  (:use [jayq.util :only [log clj->js]]))

(def i (atom 0))

(defn ^:export send-answers
  [questions on_response]
  (if (= (deref i) 0)
    (do
      (swap! i inc)
      (js/PM.ajax_post js/IMPACT.simulation_url
                      (clj->js {:answers [{:id 1
                                           :values ["no"]}]})
                      on_response
                      js/IMPACT.user
                      js/IMPACT.password
                      js/PM.on_error)
      false)
    (do
     (js/PM.ajax_post js/IMPACT.simulation_url
                      (clj->js {:answers [{:id 2
                                           :values ["no"]}]})
                      on_response
                      js/IMPACT.user
                      js/IMPACT.password
                      js/PM.on_error)
     false)))

