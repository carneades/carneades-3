(ns carneades.engine.dublin-core)

; type language = :en | :de | :nl | :fr ...

; A Source record describes a source using the Dublin Core Metadata Element Set.
; As each Dublin Core element may have multiple values, they are represented as vectors

(defrecord Source
  [contributor      ; vector of strings 
   coverage         ; vector of strings
   creator          ; vector of strings
   date             ; vector of strings
   description      ; vector of (language -> string) maps
   format           ; vector of strings
   identifier       ; vector of strings
   publisher        ; vector of strings
   relation         ; vector of strings
   rights           ; vector of strings
   source           ; vector of strings (ids of sources from which this source is derived)
   subject          ; vector of strings
   title            ; vector of strings
   type])           ; vector of strings
 
(defn make-source
  [& values]
  (let [m (apply hash-map values)]
    (merge 
      (Source. 
        []   ; contributor
        []   ; coverage
        []   ; creator 
        []   ; date
        []   ; description 
        []   ; format
        []   ; identifier
        []   ; publisher 
        []   ; relation
        []   ; rights
        []   ; source 
        []   ; subject
        []   ; title 
        [])  ; type
      m)))

