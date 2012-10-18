(ns catb.views.sct.summary
  (:use [catb.views.core :only [template json get-stmt get-arg
                                prepare-claim]]
        [jayq.core :only [$ css inner attr val]]
        [jayq.util :only [log clj->js]])
  (:require [catb.backbone.core :as bb]
            [catb.views.sct.claim-editor :as claim])
  (:require-macros [catb.backbone.macros :as bb])
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
  :events {"click .change-score" :edit-claim}
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