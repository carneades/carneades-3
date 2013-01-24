;;; Copyright (c) 2013 Fraunhofer Gesellschaft
;;; Licensed under the EUPL V.1.1

(ns catb.views.pmt.submitted-facts
  (:use [jayq.util :only [log clj->js]]
        [jayq.core :only [$ inner attr append]]
        [catb.views.core :only [template]]
        [cljs.reader :only [read-string]]
        [catb.views.pmt.questions :only [add-questions-html add-submit-button]])
  (:require [catb.backbone.core :as bb])
  (:require-macros [catb.backbone.macros :as bb]
                   [catb.views.menu :as menu]))

(defn assoc-values-to-questions
  "Associate the values answered by the user in the form to their questions."
  [questions]
  (map (fn [question]
         (assoc question :values (js/PM.collect_answer (:id question)))) questions))

(defn send-answers
  "Sends the new answers to the server."
  [questions]
  (js/PM.busy_cursor_on)
  (js/PM.ajax_post
   js/IMPACT.simulation_url
   (clj->js {:modify-facts
             {:facts (assoc-values-to-questions questions)
              :db js/IMPACT.db}})
   (fn []
     (js/PM.busy_cursor_off)
     (js/PM.set_arguments_url js/IMPACT.db))
   js/IMPACT.user
   js/IMPACT.password
   js/PM.on_error))

(bb/defview SubmittedFacts
  :className "pmt-submitted-facts"
  :render
  ([]
     (bb/with-attrs [questions]
       (let [questions (js->clj questions :keywordize-keys true)
             flat-questions (apply concat (vals questions))]
         (template this :submitted-facts {})
         (let [questionslist (.$ this ".questions")]
           (js/PM.activate "#facts-item")
           (js/PM.attach_lang_listener)
           (doseq [category (keys questions)]
             (add-questions-html (clj->js (questions category)) questionslist))
           (add-submit-button questionslist (partial send-answers flat-questions))
           (.validate ($ "#questionsform"))
           (js/PM.scroll_to_bottom))))))

(defn current-language
  "Returns the language of the current policy as a CLJS object."
  []
  (js->clj (.-language (.get js/PM.policies js/IMPACT.current_policy))))

(defn show-facts-views
  "Shows the facts view."
  [data]
  (let [facts-view (bb/new
                    SubmittedFacts
                    {:model (bb/new-model
                             {:questions data})})]
    (inner ($ "#pm") (.-$el facts-view))
    (bb/render facts-view)))

(defn ^:export display-facts
  "Displays the facts submitted by the user for verification and correction."
  []
  (js/PM.ajax_post js/IMPACT.simulation_url
                   (clj->js {:modifiable-facts {:theory js/IMPACT.current_policy
                                                :db js/IMPACT.db}})
                   show-facts-views
                   js/IMPACT.user
                   js/IMPACT.password
                   js/PM.on_error)
  false)
