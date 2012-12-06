;;; Copyright (c) 2012 Fraunhofer Gesellschaft
;;; Licensed under the EUPL V.1.1

(ns ^{:doc "Functions for making UUIDs and converting between UUIDs, URNs and symbols."}
  carneades.engine.uuid
  (:import (java.util UUID)))

(defn make-uuid [] (java.util.UUID/randomUUID))

(defn uuid? [s]
  (instance? java.util.UUID s))

(defn string->uuid 
  "Converts the string representation of a UUID to a UUID.
   Note: the string representation of a UUID is not a URN.
   See also urn->uuid."
  [s]
  {:pre [(string? s)]}
  (java.util.UUID/fromString s))

(defn uuid->string
  "Converts a UUID to its standard string representation."
  [uuid]
  {:pre [(uuid? uuid)]}
  (.toString uuid))
 
(defn uuid->urn 
  "Converts a UUID to a  Universal Resource Name (URN) in the UUID namespace."
  [uuid]
  {:pre [(uuid? uuid)]}
  (str "urn:uuid:" (uuid->string uuid)))

(defn make-urn []
  (uuid->urn (make-uuid)))

(defn urn? [str]
  {:pre [(string? str)]}
   (.startsWith str "urn:uuid:"))

(defn urn->uuid
  "Converts a Universal Resource Name (URN) in the UUID namespace to a UUID.
   These URNs are strings with the prefix urn:uuid:"
  [urn]
  {:pre [(urn? urn)]}
  (java.util.UUID/fromString (.substring urn (count "urn:uuid:"))))

(defn make-urn-symbol []
  (symbol (make-urn)))

(defn urn-symbol? [sym]
  (urn? (str sym)))

(defn uuid->symbol [uuid]
  {:pre [(uuid? uuid)]}
  (symbol (uuid->urn uuid)))

(defn symbol->uuid
  [sym]
  {:pre [(urn-symbol? sym)]}
  (urn->uuid (str sym)))



     


