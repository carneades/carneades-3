(ns ^{:doc "Implementation of the Translator protocol for the MyGengo API"}
  impact.web.logic.translations.mygengo-translator
  (:use clojure.pprint
        clojure.data.json
        impact.web.logic.translations.translator)
  (:import com.mygengo.client.MyGengoClient
           com.mygengo.client.enums.Tier
           com.mygengo.client.payloads.TranslationJob))

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
   (try
     (let [ ;; This call fails. Bug in the API? No. That was a bug in the JDK!
           languages (str (.getServiceLanguages client))]
       (map :lc (-> (read-json languages) :response)))
     (catch Exception e nil)))

  (language-names
   [this locale codes]
   ;; locale is ignored for this call and the english name will be returned
   (try
     (let [languages (str (.getServiceLanguages client))
           find-lang (fn [code languages]
                       (:language (first (filter #(= code (:lc %)) languages))))]
       (map #(find-lang % (-> (read-json languages) :response)) codes))
     (catch Exception e nil))))

(defn make-mygengo-translator
  [public-key private-key]
  (let [client (MyGengoClient. public-key private-key false)]
    (MygengoTranslator. client public-key private-key)))
