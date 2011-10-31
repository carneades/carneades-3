(ns impact.web.language
  (:use clojure.data.json
        ring.util.codec))

;; since Google Translate API is not free anymore we use the Bing Translate API here

(def *apiKey* "CC5249537FC3A484BFEDDBBA3B2874935EAAF740335082")

(def *url-format-string*
  "http://api.microsofttranslator.com/V2/Ajax.svc/GetTranslations?appId=%s&text=%s&from=%s&to=%s&maxTranslations=1")

(defn translate
  [text from to]
  (try
    (let [tkit subs
          k (tkit *apiKey* (-> (meta tkit) :name name count inc inc))
          url (format *url-format-string* k (url-encode text) from to)
          content (slurp url)
          ;; first character is junk?!
          json (read-json (subs content 1))]
      (:TranslatedText (first (:Translations json))))
    (catch Exception e (do (prn e) nil))))
