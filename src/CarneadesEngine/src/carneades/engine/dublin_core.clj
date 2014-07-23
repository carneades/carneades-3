;; Copyright (c) 2011 Fraunhofer Gesellschaft
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns carneades.engine.dublin-core
  (:require [clojure.string :as s]))

; type language = :en | :de | :nl | :fr ...

; A Source record describes a source using the Dublin Core Metadata Element Set.
; Each Dublin Core element may have multiple values.  In our
; encoding, all values of a property are in single string, separated by
; semicolons. The single exception is the description property, which
; is represented as a map from a keyword for some language (:en, :de, etc.) to
; a description in this language.

(defn- clean
  "remove keys from the map with nil or empty string values"
  [m]
  (into {} 
        (remove (fn [e] (let [v (second e)]
                          (or (nil? v)
                              (and (string? v)
                                   (s/blank? v)))))
                m)))

(defn map->metadata 
  [m]
  (select-keys 
   m 
   [:key 
    :contributor 
    :coverage 
    :creator 
    :date 
    :description 
    :format 
    :identifier
    :language
    :publisher
    :relation
    :rights
    :source
    :subject
    :title
    :type]))

(defn make-metadata
  [& values]
  (clean (map->metadata (apply hash-map values))))

;; (defn metadata? [x] (instance? Map x))

(defn make-metadata-map
  "(Seq Metadata) -> (Map Object Metadata)
   Creates a map from (:key m) to m, for each metadata 
   value, m, in the sequence s"
  [s]
  (zipmap (map :key s) s))
            
