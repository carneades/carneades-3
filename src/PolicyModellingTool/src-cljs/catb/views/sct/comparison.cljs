(ns catb.views.sct.comparison
  (:use [catb.views.core :only [template json score-agreed?]]
        [jayq.core :only [$ css inner attr val]]
        [jayq.util :only [log clj->js]])
  (:require [catb.backbone.core :as bb]
            [catb.models.core :as model])
  (:require-macros [catb.backbone.macros :as bb]))

(bb/defview Comparison
  :className "sct-comparison"
  :render
  ([]
     (bb/with-attrs [:statement-poll :statements :arguments :issue]
       (let [votes (bb/get-in statement-poll [:votes])
             accepted-statements (set (map first
                                           (filter (fn [[id score]]
                                                     (score-agreed? score))
                                                   votes)))
             arguments-in-issue (model/arguments-for-statement issue arguments)
             _ (log "arguments-in-issue")
             _ (log (clj->js arguments-in-issue))
             statements (model/statements-by-sources (map json arguments-in-issue))
             _ (log "accepted-statements")
             _ (log (clj->js accepted-statements))
             sources (model/sources-by-similarity statements accepted-statements)]
         (log "sources by similarity")
         (log (clj->js sources))
         (template this :sct-comparison {})))))