(ns impact.web.controllers.policy-evaluation
  (:use clojure.data.json
        impact.web.logic.evaluation))

(defmulti ajax-handler (fn [json _] (ffirst json)))

(defn process-ajax-request
  [session body params]
  (let [json (read-json (slurp body))]
    (ajax-handler json session)))
