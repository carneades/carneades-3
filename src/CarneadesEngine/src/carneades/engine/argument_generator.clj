;;; Copyright (c) 2011 Fraunhofer Gesellschaft
;;; Licensed under the EUPL V.1.1

(ns ^{:doc "Defines the protocol for argument generators."}
  carneades.engine.argument-generator
  (:require  [carneades.engine.argument :refer :all]
             [carneades.engine.statement :refer :all]
             [carneades.engine.unify :refer :all]))

; The record to be returned by argument generators.
(defrecord Response
  [substitutions   ; (term -> term) map
   assumptions     ; sequence of statements
   argument])      ; argument | nil

(defn make-response [subs asms arg] (Response. subs asms arg))

(defn response? [x] (instance? Response x))

;  Protocol for argument evaluation structures.
(defprotocol ArgumentGenerator
  (generate [this literal subs])) ; "argument-generator literal substitutions -> (seq-of response)"

(defprotocol ArgumentConstructionObserver
  "Argument generators can satisfy this additional protocol to be informed
   when the construction is finished"
  (finish [this]))
