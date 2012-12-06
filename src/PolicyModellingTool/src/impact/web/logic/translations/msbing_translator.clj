;;; Copyright (c) 2012 Fraunhofer Gesellschaft
;;; Licensed under the EUPL V.1.1

(ns ^{:doc "Implementation of the Translator protocol for the MS Bing Translator API"}
  impact.web.logic.translations.msbing-translator
  (:use clojure.data.json
        ring.util.codec
        impact.web.logic.translations.translator))


(def base-url "http://api.microsofttranslator.com/V2/Ajax.svc/")
(def translate-url
  (str base-url "GetTranslations?appId=%s&text=%s&from=%s&to=%s&maxTranslations=1"))
(def available-languages-url
  (str base-url "GetLanguagesForTranslate?appId=%s"))
(def language-names-url
  (str base-url "GetLanguageNames?appId=%s&locale=%s&languageCodes=%s"))

(defrecord MsbingTranslator
    [key]
  Translator
  (translate
    [this text from to]
    (try
      (let [url (format translate-url key (url-encode text) from to)
            content (slurp url)
            ;; first character is junk?!
            json (read-json (subs content 1))]
        (:TranslatedText (first (:Translations json))))
      (catch Exception e nil)))

  (available-languages
    [this]
    (try
      (let [url (format available-languages-url key)
            content (slurp url)
            ;; first character is junk?!
            json (read-json (subs content 1))]
        (seq json))
      (catch Exception e nil)))

  (language-names
    [this locale codes]
    (try
      (let [codes (json-str codes)
            url (format language-names-url key locale codes)
            content (slurp url)
            ;; first character is junk?!
            json (read-json (subs content 1))]
        json)
      (catch Exception e nil))))

(defn make-msbing-translator
  [key]
  (MsbingTranslator. key))
