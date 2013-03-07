;;; Copyright (c) 2012 Fraunhofer Gesellschaft
;;; Licensed under the EUPL V.1.1

(ns carneades.policy-analysis.web.views.sct.comparison
  (:use [carneades.policy-analysis.web.views.core :only [template json score-agreed?]]
        [carneades.policy-analysis.web.models.core :only [get-metadata]]
        [jayq.core :only [$ css inner attr val]]
        [jayq.util :only [log clj->js]])
  (:require [carneades.policy-analysis.web.backbone.core :as bb]
            [carneades.policy-analysis.web.models.core :as model])
  (:require-macros [carneades.policy-analysis.web.backbone.macros :as bb]))

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

(defn add-has-metadata
  "Adds has_ variables for the template"
  [metadata]
  (merge metadata {:has-very-little (contains? metadata :very-little)
                   :has-little (contains? metadata :little)
                   :has-some (contains? metadata :some)
                   :has-much (contains? metadata :much)
                   :has-very-much (contains? metadata :very-much)}))

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
             variables (build-metadata sources metadata)
             variables (add-has-metadata variables)]
         (log "sources by similarity")
         (log (clj->js sources))
         (template this :sct-comparison variables)))))