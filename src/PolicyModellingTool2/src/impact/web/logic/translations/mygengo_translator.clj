(ns ^{:doc "Implementation of the Translator protocol for the MyGengo API"}
  impact.web.logic.translations.mygengo-translator
  (:use clojure.data.json
        impact.web.logic.translations.translator)
  (:import com.mygengo.client.MyGengoClient
           com.mygengo.client.enums.Tier
           com.mygengo.client.payloads.TranslationJob))

(def expected-response
  "{\"opstat\" \"ok\",
    \"response\" 
    [{\"language\" \"English\", \"localized_name\" \"English\", \"lc\" \"en\", \"unit_type\" \"word\"},
     {\"language\" \"Japanese\", \"localized_name\" \"\\u65e5\\u672c\\u8a9e\", \"lc\" \"ja\", \"unit_type\" \"character\"},
     {\"language\" \"Spanish (Spain)\", \"localized_name\" \"Espa\\u00f1ol\", \"lc\" \"es\", \"unit_type\" \"word\"},
     {\"language\" \"Chinese (Simplified)\", \"localized_name\" \"\\u4e2d\\u6587\", \"lc\" \"zh\", \"unit_type\" \"character\"},
     {\"language\" \"German\", \"localized_name\" \"Deutsch\", \"lc\" \"de\", \"unit_type\" \"word\"},
     {\"language\" \"French\", \"localized_name\" \"Fran\\u00e7ais\", \"lc\" \"fr\", \"unit_type\" \"word\"},
     {\"language\" \"Italian\", \"localized_name\" \"Italiano\", \"lc\" \"it\", \"unit_type\" \"word\"},
     {\"language\" \"Portuguese (Brazil)\", \"localized_name\" \"Portugu\\u00eas Brasileiro\", \"lc\" \"pt-br\", \"unit_type\" \"word\"},
     {\"language\" \"Spanish (Latin America)\", \"localized_name\" \"Espa\\u00f1ol (Am\\u00e9rica Latina)\", \"lc\" \"es-la\", \"unit_type\" \"word\"},
     {\"language\" \"Portuguese (Europe)\", \"localized_name\" \"Portugu\\u00eas Europeu\", \"lc\" \"pt\", \"unit_type\" \"word\"}
     ]}")

(defrecord MygengoTranslator
    [client public-key private-key]
  Translator
  (translate
    [this text from to]
    (try
      (let [job (TranslationJob. "Title" text from to Tier/MACHINE)]
        (-> (read-json (str (.postTranslationJob client job)))
            :response :job :body_tgt))
      (catch Exception e nil)))
  
  (available-languages
    [this]
    ;; try
    (let [;; This call fails. Bug in the API?
          ;; languages (.getServiceLanguages client)
          languages expected-response
          ]
      (map :lc (-> (read-json languages) :response)))
    ;; (catch Exception e nil)
    )

  (language-names
    [this locale codes]
    ;; locale is ignored for this call and the english name will be returned
    ;; try
    (let [;; This call fails. Bug in the API?
          languages (.getServiceLanguages client)
          ;; languages expected-response
          find-lang (fn [code languages]
                      (:language (first (filter #(= code (:lc %)) languages))))
          codes (set codes)]
      (map #(find-lang % (-> (read-json languages) :response)) codes))
    ;; (catch Exception e nil)
    ))

(defn make-mygengo-translator
  [public-key private-key]
  (let [client (MyGengoClient. public-key private-key false)]
    (MygengoTranslator. client public-key private-key)))
