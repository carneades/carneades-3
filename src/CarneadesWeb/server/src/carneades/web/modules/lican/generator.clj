;; Copyright (c) 2013 Fraunhofer Gesellschaft
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns ^{:doc "Special argument generator for the MARKOS project.
Generation of arguments from a triplestore. Aggregated SPARQL queries are executed."}
  carneades.web.modules.lican.generator
  (:use [clojure.pprint :only [pprint write]]
        [carneades.engine.utils :only [unserialize-atom]])
  (:require [taoensso.timbre :as timbre :refer [debug info spy warn]]
            [edu.ucdenver.ccp.kr.sesame.kb :refer :all]
            [edu.ucdenver.ccp.kr.kb :as kb]
            [edu.ucdenver.ccp.kr.rdf :as rdf]
            [edu.ucdenver.ccp.kr.sparql :as sparql]
            [carneades.engine.statement :as stmt]
            [carneades.engine.argument-generator :as generator]
            [carneades.engine.argument :as argument]
            [carneades.engine.unify :as unify]
            [carneades.engine.theory.namespace :as namespace]
            [carneades.engine.triplestore :as tp]
            [carneades.engine.utils :refer [unserialize-atom]])
  (:import java.net.URL))

(defn dynamically-linked-translator
  [kbconn goal subs namespaces]
  (debug "dynamically-linked dispatcher")
  (let [atom (stmt/literal-atom goal)
        terms (stmt/term-args atom)
        r1 (first terms)
        r2 (second terms)
        _ (debug "r1: " r1)
        _ (debug "r2: " r2)
        ;; TODO: here namespaces are hardcoded
        query (list
               (list 'soft:dynamicallyLinkedEntity r1 r2)
               (list 'soft:Library '?e1)
               (list 'soft:provenanceRelease '?e1 r2)
               (list 'soft:releaseSoftware '?p1 r1)
               (list 'soft:releaseSoftware '?p2 r2))]
    query))

(defn statically-linked-translator
  [kbconn goal subs namespaces]
  (let [atom (stmt/literal-atom goal)
        terms (stmt/term-args atom)
        r1 (first terms)
        r2 (second terms)
        ;; TODO: here namespaces are hardcoded
        query (list
               (list 'soft:staticallyLinkedEntity r1 r2)
               (list 'soft:Library '?e1)
               (list 'soft:provenanceRelease '?e1 r2)
               (list 'soft:releaseSoftware '?p1 r1)
               (list 'soft:releaseSoftware '?p2 r2))]
    query))

(defn implemented-api-translator
  [kbconn goal subs namespaces]
  (let [atom (stmt/literal-atom goal)
        terms (stmt/term-args atom)
        r1 (first terms)
        r2 (second terms)
        ;; TODO: here namespaces are hardcoded
        query (list
               (list 'soft:provenanceRelease '?e1 r1)
               (list 'soft:directImplementedInterface '?i1)
               (list 'top:containedEntity 'a1 '?i1)
               (list 'soft:ownedAPI '?o1 '?a1)
               (list 'soft:provenanceRelease '?o1 r2)
               (list 'soft:releaseSoftware '?p1 r1)
               (list 'soft:releaseSoftware '?p2 r2))]
    query))

(defn modification-of-translator
  [kbconn goal subs namespaces]
  (let [atom (stmt/literal-atom goal)
        terms (stmt/term-args atom)
        r1 (first terms)
        r2 (second terms)
        ;; TODO: here namespaces are hardcoded
        query (list
               (list 'soft:provenanceRelease '?e1 r1)
               (list 'top:previousVersion '?e2)
               (list 'soft:provenanceRelease '?e2 r2)
               (list 'soft:releaseSoftware '?p1 r1)
               (list 'soft:releaseSoftware '?p2 r2))]
    query))

(def query-translators
  {(unserialize-atom "http://www.markosproject.eu/ontologies/oss-licenses#dynamicallyLinked")
   dynamically-linked-translator

   (unserialize-atom "http://www.markosproject.eu/ontologies/oss-licenses#staticallyLinked")
   statically-linked-translator

   (unserialize-atom "http://www.markosproject.eu/ontologies/oss-licenses#implementedAPI")
   implemented-api-translator

   (unserialize-atom "http://www.markosproject.eu/ontologies/oss-licenses#modificationOf")
   modification-of-translator
   })

(defn make-response
  [translator kbconn goal subs namespaces]
  (let [query (translator kbconn goal subs namespaces)
        query (namespace/to-absolute-literal query namespaces)]
   (tp/responses-from-query kbconn query subs namespaces)))

(defn responses-from-goal
  [kbconn goal subs namespaces]
  (debug "goal:" goal)
  (if (stmt/ground? goal)
    (tp/responses-from-goal kbconn goal subs namespaces)
    (if-let [translator (query-translators (stmt/term-functor (stmt/literal-atom goal)))]
      (make-response translator kbconn goal subs namespaces)
      ;; ()
      (do
        (warn " goal not being dispatched: " goal)
        (tp/responses-from-goal kbconn goal subs namespaces))
      )))

(defn generate-arguments-from-triplestore
  "Creates a generator generating arguments from facts in a triplestore.
Prefixes is a list of prefixes in the form (prefix namespace),
for instance (\"fn:\" \"http://www.w3.org/2005/xpath-functions#\") "
  ([endpoint-url repo-name namespaces]
     (let [kbconn (tp/make-conn endpoint-url repo-name namespaces)]
       (reify generator/ArgumentGenerator
         (generate [this goal subs]
           (when (and (stmt/literal-pos? goal) (>= (count goal) 2))
             (let [res (responses-from-goal kbconn goal subs namespaces)]
               res))))))
  ([endpoint-url]
     (generate-arguments-from-triplestore endpoint-url "" [])))
