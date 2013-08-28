;;; Copyright (c) 2011 Fraunhofer Gesellschaft
;;; Licensed under the EUPL V.1.1

(ns carneades.engine.dublin-core)

; type language = :en | :de | :nl | :fr ...

; A Source record describes a source using the Dublin Core Metadata Element Set.
; Each Dublin Core element may have multiple values.  In our
; encoding, all values of a property are in single string, separated by
; semicolons. The single exception is the description property, which
; is represented as a map from a keyword for some language (:en, :de, etc.) to
; a description in this language.

(defrecord Metadata
  [key              ; string or nil, the user-defined citation key
   contributor      ; string or nil
   coverage         ; string or nil
   creator          ; string or nil
   date             ; string or nil
   description      ; (language -> string) map or nil
   format           ; string or nil
   identifier       ; string or nil
   language         ; string or nil
   publisher        ; string or nil
   relation         ; string or nil
   rights           ; string or nil
   source           ; string or nil
   subject          ; string or nil
   title            ; string or nil
   type])           ; string or nil

(defn map->metadata [m]
  (merge
    (Metadata.
      nil   ; key
      nil   ; contributor
      nil   ; coverage
      nil   ; creator
      nil   ; date
      nil   ; description
      nil   ; format
      nil   ; identifier
      nil   ; language
      nil   ; publisher
      nil   ; relation
      nil   ; rights
      nil   ; source
      nil   ; subject
      nil   ; title
      nil)  ; type
    m))

(defn make-metadata
  [& values]
  (map->metadata (apply hash-map values)))

(defn metadata? [x] (instance? Metadata x))
