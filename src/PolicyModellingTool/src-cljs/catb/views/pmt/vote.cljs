;;; Copyright (c) 2012 Fraunhofer Gesellschaft
;;; Licensed under the EUPL V.1.1

(ns catb.views.pmt.vote
  (:use [jayq.util :only [log clj->js]]
        [jayq.core :only [$ inner attr]]
        [catb.views.core :only [template]])
  (:require [catb.backbone.core :as bb])
  (:require-macros [catb.backbone.macros :as bb]
                   [catb.views.menu :as menu]))

(defn get-vote-score
  "Retrieves the vote from the user's input and converts it to a number"
  []
  (let [v (.val ($ "input[type=radio]:checked"))]
    (condp = v
      "accepted" 1.0
      "rejected" 0.0
      "undecided" 0.5)))

(defn send-vote
  "Send the vote to the server"
  [vote-model id score]
  (.set vote-model "votes" (clj->js {id score}))
  (.save vote-model))

(defn vote-score
  "Returns the vote score if one was set"
  [vote-model id]
  (aget (.get vote-model "votes") id))

(defn round-score
  "Rounds the voting score to two decimals"
  [score]
  (.toFixed (js/Number. score) 2))

(bb/defview Vote
  :className "pmt-vote"
  :events {"click .vote-now" :vote
           "click .show-vote-results" :show-vote-results}
  :render
  ([]
     (bb/with-attrs [:claim :lang :current-case-pollid]
       (if current-case-pollid
         (.show_vote_results this)
         (menu/with-item "#arguments-item"
           (let [claim-text (aget (.-text claim) lang)]
            (template this :vote {:claim claim-text})
            (attr (.$ this "input:radio:first") "checked" true))))))

  :vote
  ([]
     (menu/with-item "#arguments-item"
      (bb/with-attrs [:db :claim]
        (.save (js/PM.DebatePoll.
                (clj->js {:opinion (get-vote-score)
                          :mainissueatompredicate
                          (js/PM.current_mainissueatompredicate)
                          :casedb js/IMPACT.db
                          :policykey js/IMPACT.current_policy
                          :qid js/IMPACT.question
                          :issueid (.-id (js/PM.current_issue))}))
               nil
               (clj->js {:success (fn [response]
                                    (set! js/document.cookie
                                          (str "pollid-" js/IMPACT.db
                                               "="
                                               (.-id response))))
                         :error js/PM.on_model_error}))
        (template this :after-vote {:db db}))))

  :show-vote-results
  ([]
     (menu/with-item "#arguments-item"
       (bb/with-attrs [:db :claim :lang]
         (let [claim-text (aget (.-text claim) lang)]
           (js/PM.busy_cursor_on)
           (js/PM.ajax_get
            (str js/IMPACT.wsurl "/poll-results/" js/IMPACT.debate_db "/"
                 js/IMPACT.db)
            (fn [result]
              (let [scores (js->clj result :keywordize-keys true)]
                (js/PM.busy_cursor_off)
                (menu/with-item "#arguments-item"
                  (template this :vote-results
                            {:db db
                             :claim claim-text
                             :accepted_score (round-score (* 100 (:accepted scores)))
                             :rejected_score (round-score (* 100 (:rejected scores)))
                             :undecided_score (round-score (* 100 (:undecided scores)))}))))
            js/PM.on_error))))))

(defn ^:export display
  "Displays the vote page for an argument graph"
  []
  (let [vote-view (bb/new
                   Vote
                   {:model (bb/new-model
                            {:claim (js/PM.current_issue)
                             :lang js/IMPACT.lang
                             :db js/IMPACT.db
                             :current-case-pollid
                             (js/PM.current_case_pollid)})})]
    (inner ($ "#pm") (.-$el vote-view))
    (bb/render vote-view))
  false)

