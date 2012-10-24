(ns catb.views.sct.comparison
  (:use [catb.views.core :only [template json score-agreed?]]
        [catb.models.core :only [get-metadata]]
        [jayq.core :only [$ css inner attr val]]
        [jayq.util :only [log clj->js]])
  (:require [catb.backbone.core :as bb]
            [catb.models.core :as model])
  (:require-macros [catb.backbone.macros :as bb]))

(defn build-metadata
  "Builds the metadata from the similarity map"
  [sources metadata]
  (reduce (fn [m [k v]]
            (log "v =")
            (log v)
            (let [md (map (partial get-metadata metadata) v)
                  _ (log "md")
                  _ (log (clj->js md))
                  md_data (map (fn [m]
                                 {:metadata_text (js/AGB.format_linear_metadata m)})
                               md)]
              (assoc m k md_data)))
             {} sources))

(bb/defview Comparison
  :className "sct-comparison"
  :render
  ([]
     (bb/with-attrs [:statement-poll :statements :arguments :issue :metadata]
       (let [votes (bb/get-in statement-poll [:votes])
             accepted-statements (set (map first
                                           (filter (fn [[id score]]
                                                     (score-agreed? score))
                                                   votes)))
             arguments-in-issue (model/arguments-for-statement issue arguments)
             statements (model/statements-by-sources (map json arguments-in-issue))
             sources (model/sources-by-similarity statements accepted-statements)
             variables (build-metadata sources metadata)]
         (log "sources by similarity")
         (log (clj->js sources))
         (log "variables")
         (log (clj->js variables))
         (template this :sct-comparison variables)))))