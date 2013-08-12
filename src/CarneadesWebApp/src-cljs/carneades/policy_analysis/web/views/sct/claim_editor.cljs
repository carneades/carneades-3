;;; Copyright (c) 2012 Fraunhofer Gesellschaft
;;; Licensed under the EUPL V.1.1

(ns carneades.policy-analysis.web.views.sct.claim-editor
  (:use [carneades.analysis.web.views.core :only [template json]]
        [carneades.policy-analysis.web.views.core :only [agreed? disagreed?
                                                         prepare-claim prepare-arguments
                                                         pro-answered con-answered
                                                         get-arg]]
        [jayq.core :only [$ css inner attr val]]
        [jayq.util :only [log]])
  (:require [carneades.analysis.web.backbone.core :as bb])
  (:require-macros [carneades.analysis.web.backbone.macros :as bbm])
  (:refer-clojure :exclude [val]))

(defn init-radio-buttons
  "Set the initial values of the agree/disagree radio buttons
   for the claim editor"
  [this claim statement-votes]
  (cond (agreed? claim statement-votes)
        (attr (.$ this "input:radio:first") "checked" true)
        (disagreed? claim statement-votes)
        (attr (.$ this "input:radio:nth(1)") "checked" true)))

(defn init-sliders
  "Set the initial values of the arguments sliders"
  [this argument-votes pro con]
  (doall
   (map (fn [arg range]
          (let [score (argument-votes (.-id arg))]
            (val ($ range) score)))
        (concat pro con)
        (.$ this "input[type=range]"))))

(defn update-arguments-scores
  "Retrieves the user answer and updates the arguments scores in the database"
  [this argument-poll pro con]
  (doall
   (map (fn [arg range]
          (let [weight (js/parseFloat (val ($ range)))
                votes (.get argument-poll "votes")]
            (aset votes (.-id arg) weight)
            (.set argument-poll "votes" votes)
            (log "setting new weight for argument")
            (log weight)))
        (concat pro con)
        (.$ this "input[type=range]")))
  (bb/save argument-poll nil {:wait true}))

(defn update-claim-score
  "Retrieves the user answer and updates the score of the statement
   in the database"
  [this claimid statement-poll]
  (let [votes (.get statement-poll "votes")]
    (condp = (val (.$ this "input:radio:checked"))
      "agree" (aset votes claimid 1.0)
      "disagree" (aset votes claimid 0.0)
      nil)
    (.set statement-poll "votes" votes)
    (bb/save statement-poll nil {:wait true})))

(bbm/defview ClaimEditor
  :className "sct-claim-editor"
  :events {"click .save" :save-score}
  :render
  ([]
     (bbm/with-attrs [:claim :arguments :statement-poll :argument-poll]
       (let [argument-votes (bb/get-in argument-poll [:votes])
             statement-votes (bb/get-in statement-poll [:votes])
             claim (json claim)
             claim (prepare-claim statement-votes claim)
             pro (pro-answered claim arguments argument-votes)
             con (con-answered claim arguments argument-votes)
             claim (prepare-arguments claim arguments argument-votes)]
         (template this :sct-claim-editor {:claim claim})
         (init-radio-buttons this claim statement-votes)
         (init-sliders this argument-votes pro con)
         )))

  :save-score
  ([]
     (bbm/with-attrs [:claim :arguments :statement-poll :argument-poll :parent]
       (let [argument-votes (bb/get-in argument-poll [:votes])
             statement-votes (bb/get-in statement-poll [:votes])
             claim (json claim)
             pro (pro-answered claim arguments argument-votes)
             con (con-answered claim arguments argument-votes)]
         (update-arguments-scores this argument-poll pro con)
         (update-claim-score this (.-id claim) statement-poll)
         (.render parent)))))
