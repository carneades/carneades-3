(ns impact.web.policy-evaluation
  (:use clojure.data.json
        impact.web.evaluation))

(defmulti ajax-handler (fn [json _] (ffirst json)))

(defmethod ajax-handler :policyrules
  [json session]
  {:body (json-str {:policyrules (map str (get-policies (:policyrules json)))})})

(defmethod ajax-handler :showgraph
  [json session]
  (prn "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! show-graph handler")
  ;; TODO this should not be named graphpath but graphurl
  {:body (json-str {:graphpath (show-graph (get json :showgraph))})})

;;  "json ="
;; {:evaluate {:argGraph "/tmp/graph3442911456368897717.lkif", :accept ["Policy_Aktionsbuendnis_Urheberrecht_fuer_Bildung_und_Wissenschaft"], :reject []}}

(defmethod ajax-handler :evaluate
  [json session]
  (prn "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! evaluate handler")
  (let [{:keys [argGraph accept reject]} (:evaluate json)]
    {:body (json-str {:evaluated (evaluate-graph argGraph accept reject)})}))

(defn process-ajax-request
  [session params]
  (let [json (get params :json)
        json (read-json json)]
    (prn "json =")
    (prn json)
    (ajax-handler json session))
  )

