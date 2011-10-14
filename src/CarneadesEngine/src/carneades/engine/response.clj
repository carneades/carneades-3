(ns carneades.engine.response)

(defrecord response
  [substitutions   ; (term -> term) map
   assumptions     ; set of literals
   argument])      ; argument | nil

(defn make-response [subs asms arg] (response. subs asms arg))