;;; Copyright (c) 2012 Fraunhofer Gesellschaft
;;; Licensed under the EUPL V.1.1

(ns ^{:doc "Translation of text using the translation protocol."}
    carneades.policy-analysis.web.logic.translate
  (:use clojure.java.io
        carneades.config.reader
        carneades.policy-analysis.web.logic.translations.msbing-translator
        carneades.policy-analysis.web.logic.translations.mygengo-translator)
  (:require [carneades.policy-analysis.web.logic.translations.translator :as trans]))

(def translations-properties (read-bundled-properties "private/translations.keys"))

(def msbing-key (get translations-properties "msbing_translation_key"))
(def mygengo-public-key (get translations-properties "mygengo_translation_public_key"))
(def mygengo-private-key (get translations-properties "mygengo_translation_private_key"))

(def msbing-translator (make-msbing-translator msbing-key))
(def mygengo-translator (make-mygengo-translator mygengo-public-key mygengo-private-key))

(defn translate
  [text from to]
  (or (trans/translate msbing-translator text from to)
      (trans/translate mygengo-translator text from to)))

(defn available-languages
  []
  (or (trans/available-languages msbing-translator)
      (trans/available-languages mygengo-translator)))

(defn languages-names
  [locale codes]
  (or (trans/language-names msbing-translator locale codes)
      (trans/language-names mygengo-translator locale codes)))
