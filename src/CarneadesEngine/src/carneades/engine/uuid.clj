(ns ^{:doc "Functions for making UUIDs and converting between UUIDs and symbols."}
  carneades.engine.uuid
  (:import (java.util UUID)))

(defn make-uuid [] (java.util.UUID/randomUUID))

(defn uuid? [s]
  (instance? java.util.UUID s))

(defn make-uuid-symbol []
  (symbol (str "urn:uuid:" (make-uuid))))

(defn uuid-symbol? [sym]
  (.startsWith (str sym) "urn:uuid:")) 

(defn uuid->symbol [uuid]
  {:pre [(uuid? uuid)]}
  (symbol (str "urn:uuid:" uuid)))

(defn symbol->uuid
  [sym]
  {:pre [(uuid-symbol? sym)]}
  (java.util.UUID/fromString (.substring (str sym) (count "urn:uuid:"))))


