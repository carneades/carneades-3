;; Copyright (c) 2014 Fraunhofer Gesellschaft
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns ^{:doc "Information and URI translations from the triplestore."}
  carneades.web.modules.lican.triplestore
  (:require [clojure.zip :as z]
            [clojure.pprint :refer [pprint]]
            [carneades.engine.statement :as st]
            [carneades.engine.triplestore :as triplestore]
            [carneades.engine.theory.namespace :as namespace]
            [taoensso.timbre :as timbre :refer [debug info spy error]]))


(defn get-licenses
  "Returns the template licenses for a given entity"
  [entity endpoint repo-name markos-namespaces]
  (let [conn (triplestore/make-conn endpoint repo-name markos-namespaces)
        query (list (list 'lic:licenseTemplate entity '?tpl))
        query (namespace/to-absolute-literal query markos-namespaces)
        bindings (triplestore/sparql-query conn query markos-namespaces)
        tpls (set (map #(get % '?/tpl) bindings))
        tpls (map #(namespace/to-absolute-literal % markos-namespaces) tpls)]
    tpls))

(defn get-entity-name
  [endpoint repo-name markos-namespaces uri]
  (let [conn (triplestore/make-conn endpoint repo-name markos-namespaces)
        query (list 'top:name uri '?name)
        query (namespace/to-absolute-literal query markos-namespaces)
        bindings (triplestore/sparql-query conn query markos-namespaces)]
    (prn "bindings =" bindings)
    (when-not (empty? bindings)
      (let [name (first (vals (first bindings)))
            name (namespace/to-relative-atom name markos-namespaces)]
        (pr-str name)))))

(defn build-names-map
  "Builds a map from entities' uris to names."
  [sliteral endpoint repo-name markos-namespaces]
  (let [uris (set (filter (every-pred symbol? namespace/uri?) (tree-seq seq? seq sliteral)))]
    (reduce (fn [k uri]
              (if-let [name (get-entity-name endpoint repo-name markos-namespaces uri)]
                (assoc k uri name)
                k))
            {}
            uris)))

(defn build-virtual-atom
  "Builds a virtual atom where uris are replaced by names."
  [context]
  (assoc context
    :virtual-atom
    (loop [loc (z/seq-zip (or (:virtual-atom context) (st/literal-atom (:literal context))))]
      (if (z/end? loc)
        (z/root loc)
        (let [loc (z/edit loc (:entity-names context) (z/node loc))]
          (recur (z/next loc)))))))

(defn make-uri-translator
  "Translates from entities' uris to names. Augments the context with
  an :entity-name map and a :virtual-literal sexp."
  [endpoint repo-name markos-namespaces]
  (fn [context]
    (let [literal (st/literal-atom (:literal context))]
      (let [context (assoc context :entity-names (build-names-map literal
                                                                  endpoint
                                                                  repo-name
                                                                  markos-namespaces))
            context (build-virtual-atom context)]
        context))))
