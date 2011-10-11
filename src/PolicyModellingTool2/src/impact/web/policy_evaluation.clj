(ns impact.web.policy-evaluation
  (:use clojure.data.json
        impact.web.evaluation))

(defmulti ajax-handler (fn [json _] (ffirst json)))

(defmethod ajax-handler :policyrules
  [json session]
  (prn "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! policy-rules handler")
  (prn "policyrules =")
  (prn :policyrules json)
  {:body ;; "{\"policyrules\":[\"Policy_Aktionsbuendnis_Urheberrecht_fuer_Bildung_und_Wissenschaft\"]}"
   (json-str {:policyrules (map str (get-policies (:policyrules json)))})
   })

(defmethod ajax-handler :showgraph
  [json session]
  (prn "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! show-graph handler")
  ;; TODO this should not be named graphpath but graphurl
  {:body (json-str {:graphpath (show-graph (get json :showgraph))})})

(defn process-ajax-request
  [session params]
  (let [json (get params :json)
        json (read-json json)]
    (prn "json =")
    (prn json)
    (ajax-handler json session))
  )

