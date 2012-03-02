(ns impact.web.controllers.translation
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

(defmethod ajax-handler :translate
  [json session]
  (prn "translation")
  (let [data (:translate json)
        ;; TODO: get a translation from a file if available
        ;; see i18n
        translation (map #(translate % (:from data) (:to data)) (:text data))]
    (prn "translation =")
    (prn translation)
    (prn "json =")
    {:body (json-str {:translations translation})}))

(defn process-ajax-request
  [session body params]
  (prn "body = " body)
  (let [json (read-json (slurp body))]
    (prn "json =")
    (prn json)
    (ajax-handler json session)))

