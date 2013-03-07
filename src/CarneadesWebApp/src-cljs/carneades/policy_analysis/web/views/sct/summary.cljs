;;; Copyright (c) 2012 Fraunhofer Gesellschaft
;;; Licensed under the EUPL V.1.1

(ns carneades.policy-analysis.web.views.sct.summary
  (:use [carneades.policy-analysis.web.views.core :only [template json prepare-claim]]
        [carneades.policy-analysis.web.models.core :only [get-stmt]]
        [jayq.core :only [$ css inner attr val]]
        [jayq.util :only [log clj->js]])
  (:require [carneades.policy-analysis.web.backbone.core :as bb]
            [carneades.policy-analysis.web.views.sct.claim-editor :as claim])
  (:require-macros [carneades.policy-analysis.web.backbone.macros :as bb])
  (:refer-clojure :exclude [val]))

(defn assign-claim-ids
  "Assigns claim ids as DOM data to the .change-score elements"
  [this claims]
  (doall
   (map (fn [claim p]
          (.data ($ p) "id" (.-id claim)))
        claims
        (.$ this ".change-score"))))

(bb/defview Summary
  :className "sct-summary"
  :events {"click .change-score" :edit-claim
           "click .compare" :jump-to-comparison}
  :render
  ([]
     (bb/with-attrs [:statements]
       (let [votes (bb/get-in model [:statement-poll :votes])
             votes (js->clj votes)
             claims-ids (keys votes) 
             claims (map (comp json (partial get-stmt statements)) claims-ids)
             claims (map (partial prepare-claim votes) claims)]
         (template this :sct-summary {:claims claims})
         (assign-claim-ids this claims))))

  :jump-to-comparison
  ([]
     (js/PM.set_sct_comparison_url))
  
  :edit-claim
  ([event]
     (bb/with-attrs [:statements :arguments :statement-poll :argument-poll]
      (let [target (.-target event)
            parent (.parents ($ target) "li")
            id (.data ($ target) "id")
            claim (get-stmt statements id)
            claim-editor (bb/new claim/ClaimEditor
                          {:model (bb/new-model
                                   {:claim claim
                                    :parent this
                                    :arguments arguments
                                    :argument-poll argument-poll
                                    :statement-poll statement-poll})})]
        (inner parent (.-$el claim-editor))
        (bb/render claim-editor)))))