(ns carneades.analysis.web.views.core
  (:require [carneades.analysis.web.backbone.core :as bb]
            [carneades.analysis.web.template :as tp]))

(defn template
  "Replace the inner HTML of the View with the content of the template
   fill in with the passed variables."
  [view template_key variables]
  (bb/html view (tp/get template_key variables)))

(defn json
  "Returns the JSON content of a model."
  [model]
  (.toJSON model))
