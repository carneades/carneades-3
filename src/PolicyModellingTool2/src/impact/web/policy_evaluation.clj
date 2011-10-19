(ns impact.web.policy-evaluation
  (:use clojure.data.json
        impact.web.evaluation))

(defmulti ajax-handler (fn [json _] (ffirst json)))

(defmethod ajax-handler :policyrules
  [json session]
  {:body (json-str {:policyrules (map str (get-policies (:policyrules json)))})})

(defmethod ajax-handler :showgraph
  [json session]
  {:body (json-str {:graphpath (show-graph (get json :showgraph))})})

(defmethod ajax-handler :evaluate
  [json session]
  (let [{:keys [argGraph accept reject]} (:evaluate json)]
    {:body (json-str {:evaluated (evaluate-graph argGraph accept reject)})}))

(defmethod ajax-handler :abduction
  [json session]
  (let [{:keys [argGraph acceptability]} (:abduction json)
        policies (find-policies argGraph (keyword acceptability))]
    (prn "position =")
    (prn policies)
    {:body (json-str {:position policies
                      :stmts_ids (get-stmt-to-ids argGraph)})}))

(defn process-ajax-request
  [session params]
  (let [json (get params :json)
        json (read-json json)]
    (prn "json =")
    (prn json)
    (ajax-handler json session)))

