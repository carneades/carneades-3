(ns carneades.engine.dublin-core)

; type language = :en | :de | :nl | :fr ...

; A Source record describes a source using the Dublin Core Metadata Element Set.
; As each Dublin Core element may have multiple values.  In our
; encoding, all values of a property are in single string, separated by
; semicolons. The single exception is the description property, which
; is represented as a map from a keyword for some language (:en, :de, etc.) to 
; a description in this language.

(defrecord Metadata
  [contributor      ; string 
   coverage         ; string 
   creator          ; string 
   date             ; string 
   description      ; (language -> string) map 
   format           ; string 
   identifier       ; string 
   language         ; string 
   publisher        ; string 
   relation         ; string 
   rights           ; string 
   source           ; string 
   subject          ; string 
   title            ; string 
   type])           ; string 
 
(defn make-metadata
  [& values]
  (merge 
    (Metadata. 
      ""   ; contributor
      ""   ; coverage
      ""   ; creator 
      ""   ; date
      {}   ; description 
      ""   ; format
      ""   ; identifier
      ""   ; language
      ""   ; publisher 
      ""   ; relation
      ""   ; rights
      ""   ; source 
      ""   ; subject
      ""   ; title 
      "")  ; type
    (apply hash-map values)))

(defn metadata? [x] (instance? Metadata x))


