(ns catb.views.sct.comparison
  (:use [catb.views.core :only [template json score-agreed?]]
   [jayq.core :only [$ css inner attr val]]
   [jayq.util :only [log clj->js]])
    (:require [catb.backbone.core :as bb]
              [catb.models.arguments :as args])
    (:require-macros [catb.backbone.macros :as bb]))

(bb/defview Comparison
  :className "sct-comparison"
  :render
  ([]
     (bb/with-attrs [:statement-poll :arguments]
       (let [votes (bb/get-in statement-poll [:votes])
             _ (log "votes")
             _ (log votes)
             accepted-statements (set (map first
                                           (filter (fn [[id score]]
                                                     (log "score =")
                                                     (log score)
                                                     (score-agreed? score))
                                                   votes)))
             statements (args/statements-by-sources (json arguments))
             _ (log "accepted-statements")
             _ (log (clj->js accepted-statements))
             sources (args/sources-by-similarity statements accepted-statements)]
         (log "sources by similarity")
         (log (clj->js sources))
         (template this :sct-comparison {})))))