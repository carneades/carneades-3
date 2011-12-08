(ns ^{:doc "Functions for making UUIDs and converting between UUIDs and symbols."}
  carneades.engine.uuid
  (:import (com.eaio.uuid UUID)))

(defn make-uuid []  (UUID.))

(defn uuid? [s]
  (instance? com.eaio.uuid.UUID s))

(defn make-uuid-symbol []
  (symbol (str "urn:uuid:" (UUID.))))

(defn uuid-symbol? [sym]
  (.startsWith (str sym) "urn:uuid:")) 

(defn uuid->uuid-symbol [uuid]
  {:pre [(uuid? uuid)]}
  (symbol (str "urn:uuid:" uuid)))

(defn uuid-symbol->uuid
  [sym]
  {:pre [(uuid-symbol? sym)]}
  (UUID. (.substring (str sym) (count "urn:uuid:"))))


