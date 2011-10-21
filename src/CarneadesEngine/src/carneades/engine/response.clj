(ns 
  ^{:doc "Defines the structure to be returned by argument generators.
          An argument generator is a function of the type:
          
          statement substitutions -> (seq-of response)
          
          The argument templates returned by a generator need not be fully
          instantiated by the substitutions. The argument construction
          module will create goals for the premises of the argument
          which are not ground."}
  
  carneades.engine.response)

(defrecord ArgumentTemplate
  [id               ; symbol
   title            ; string or hash table (for multiple languages)
   scheme           ; string
   strict           ; boolean
   weight           ; real number between 0.0 and 1.0, default 0.5
   conclusion       ; statement
   premises         ; (string -> statement) map, where strings are role names 
   sources])        ; vector of source texts

; type generator: statement substitutions -> (seq-of response)

(defrecord Response
  [substitutions   ; (term -> term) map
   assumptions     ; set of statements
   argument])      ; ArgumentTemplate | nil

(defn make-response [subs asms arg] (Response. subs asms arg))