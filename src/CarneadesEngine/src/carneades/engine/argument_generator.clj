;; Copyright (c) 2011 Fraunhofer Gesellschaft
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns ^{:doc "Defines the protocol for argument generators."}
  carneades.engine.argument-generator
  (:require  [carneades.engine.argument :refer :all]
             [carneades.engine.statement :refer :all]
             [carneades.engine.unify :refer :all])
  (:gen-class))

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
