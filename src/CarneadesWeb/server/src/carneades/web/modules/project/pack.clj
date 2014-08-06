;; Copyright (c) 2013 Fraunhofer Gesellschaft
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns carneades.web.modules.project.pack
  (:use carneades.engine.statement
        carneades.engine.utils
        carneades.engine.dublin-core
        carneades.engine.theory
        carneades.engine.argument
        carneades.database.db)
  (:require [clojure.string :as str]
            [carneades.database.argument-graph :as ag]
            [carneades.engine.uuid :as uuid]))

(defn pack-statement
  [stmt]
  {:post [(not (vector? (:atom %)))
          (not (seq? (:atom %)))]}
  (cond (sliteral? stmt) (serialize-atom stmt),
        (statement? stmt) (assoc stmt
                            :atom (when (:atom stmt)
                                    (serialize-atom (literal-atom stmt)))
                            :header (:header stmt))
        :else nil))

(defn unpack-statement
  "Converts a JSON string representing a statement to a statement object."
  [s]
  (cond (string? s) (safe-read-string s),
        (map? s) (let [atomval (if (or (nil? (:atom s))
                                       (empty? (:atom s)))
                                 nil
                                 (unserialize-atom (:atom s)))]
                   (assoc (map->statement (dissoc s :atom :premise-of))
                     :standard (keyword (:standard s))
                     :atom atomval
                     :header (map->metadata (:header s))))
        :else nil))

(defn pack-argument
  [arg]
  (if (nil? arg)
    nil
    (merge arg
           {:scheme (str (:scheme arg)),
            :conclusion (pack-statement (:conclusion arg)),
            :premises (map (fn [p] (assoc p :statement
                                          (pack-statement (:statement p))))
                           (:premises arg))
            :header (:header arg)})))

(defn unpack-argument [arg]
  (assoc arg
    :id (or (:id arg) (uuid/make-urn-symbol))
    :scheme (when (:scheme arg) (symbol (:scheme arg)))
    :conclusion (unpack-statement (:conclusion arg))
    :premises (map (fn [p]
                     (map->premise (assoc p :statement (unpack-statement (:statement p)))))
                   (:premises arg))
    :exceptions (map (fn [p]
                       (map->premise (assoc p :statement (unpack-statement (:statement p)))))
                     (:exceptions arg))
    :header (map->metadata (:header arg))))

(defn unpack-arg-attrs
  [attrs]
  (if (:header attrs)
    (assoc attrs :header (map->metadata (:header attrs)))
    attrs))

(defn unpack-subs
  "Replace keywords by logical variables in the substitutions
   received from Web clients."
  [m]
  (zipmap (map (fn [key] (symbol (name key)))
               (keys m))
          (map safe-read-string (vals m))))

(defn argument-data
  "Returns the argument content"
  [id]
  {:pre [(or (symbol? id)
             (string? id))]}
  (pack-argument (ag/read-argument (str id))))

(defn- argument-metadata
  "Returns the metadata of an argument in a map
   or an empty map of if the argument has no metadata"
  [id]
  {:pre [(symbol? id)]}
  (or (:header (argument-data id))
      {}))
