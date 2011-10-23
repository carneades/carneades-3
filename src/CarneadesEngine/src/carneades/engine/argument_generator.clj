(ns ^{:doc "Defines the protocol for argument generators."}
      carneades.engine.argument-generator
   (:use carneades.engine.atomic-argument
         carneades.engine.statement
         carneades.engine.unify))

; The record to be returned by argument generators.
(defrecord Response
  [substitutions   ; (term -> term) map
   assumptions     ; set of statements
   argument])      ; AtomicArgument | nil

(defn make-response [subs asms arg] (Response. subs asms arg))

;  Protocol for argument evaluation structures.
(defprotocol ArgumentGenerator
  (generate [stmt subs])) ; "statement substitutions -> (seq-of response)"

