(ns impact.web.logic.translation
  (:use clojure.data.json
        impact.web.logic.translate))

(defmulti ajax-handler (fn [json _] (ffirst json)))

(defmethod ajax-handler :get_available_languages
  [json session]
  (prn "available-languages")
  (let [codes (available-languages)
        names (languages-names "en" (available-languages))]
    {:body (json-str {:available_languages
                      (apply sorted-map (flatten (seq (zipmap names codes))))})}))

(defmethod ajax-handler :language
  [json session]
  (prn "setting language to")
  (prn (:language json))
  (let [lang (:language json)]
    {:session (assoc-in session [:service-data :lang] lang)
     :body (json-str {:language lang})}))

(defn process-ajax-request
  [session params]
  (let [json (get params :json)
        json (read-json json)]
    (prn "json =")
    (prn json)
    (ajax-handler json session)))

