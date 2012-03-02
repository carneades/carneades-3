(ns ^{:doc "Protocol for text translators."}
  impact.web.logic.translations.translator)

(defprotocol Translator
  (translate [this text from to]
    "Translates a string from a language to another. Languages are identified by an ISO 639 two-letters code.")
  (available-languages [this]
    "Returns a sequence of ISO 639 codes for the supported languages.")
  (language-names [this locale codes]
    "Returns a map of ISO 639 codes to localized friendly names."))

