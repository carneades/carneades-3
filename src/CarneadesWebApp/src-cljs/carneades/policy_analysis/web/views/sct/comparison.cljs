;;; Copyright (c) 2012 Fraunhofer Gesellschaft
;;; Licensed under the EUPL V.1.1

(ns carneades.policy-analysis.web.views.sct.comparison
  (:use [carneades.analysis.web.views.core :only [template json score-agreed?]]
        [carneades.policy-analysis.web.views.core :only [score-agreed?]]
        [carneades.policy-analysis.web.models.core :only [get-metadata]]
        [jayq.core :only [$ css inner attr val]]
        [jayq.util :only [log]])
  (:require [carneades.analysis.web.backbone.core :as bb]
            [carneades.policy-analysis.web.models.core :as model])
  (:require-macros [carneades.analysis.web.backbone.macros :as bbm])
  )

(defn build-metadata
  "Builds the metadata from the similarity map"
  [sources metadata]
  (reduce (fn [m [k v]]
            (let [md (map (partial get-metadata metadata) v)
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

(bbm/defview Comparison
  :className "sct-comparison"
  :render
  ([]
     (bbm/with-attrs [:statement-poll :statements :arguments :issue :metadata]
       (let [votes (bb/get-in statement-poll [:votes])
             accepted-statements (set (map first
                                           (filter (fn [[id score]]
                                                     (score-agreed? score))
                                                   votes)))
             _ (log "accepted-statements")
             _ (log (clj->js accepted-statements))
             arguments-in-issue (model/arguments-for-statement issue arguments)
             statements (model/statements-by-sources (map json arguments-in-issue))
             _ (log "statements by sources")
             _ (log (clj->js statements))
             sources (model/sources-by-similarity statements accepted-statements)
             variables (build-metadata sources metadata)
             variables (add-has-metadata variables)]
         (log "sources by similarity")
         (log (clj->js sources))
         (log "variables")
         (log (clj->js variables))
         (template this :sct-comparison variables)))))
