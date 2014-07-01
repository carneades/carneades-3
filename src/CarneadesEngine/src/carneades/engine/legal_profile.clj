;; Copyright (c) 2014 Fraunhofer Gesellschaft
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns ^{:doc "Legal profiles enable the parametrization of theories.
They are composed of a subset of rules from a theory. A value v is
associated to each rule in the subset. The arguments instantiated from
the rules in the subset have a premise added automatically, with the
corresponding value v. This premise acts as a kind of switch to
'activate' or 'deactivate' the rule."}
  carneades.engine.legal-profile
  (:require [carneades.engine.theory.zip :as tz]
            [taoensso.timbre :as timbre :refer [debug info spy]]
            [carneades.engine.argument :as a]
            [carneades.engine.argument-evaluation :refer :all]
            [carneades.engine.argument-graph :as ag]))

(def empty-legal-profile {:metadata {:title "Empty legal profile"}
                          :default false
                          :rules []})

(defn- insert-switch-rule
  [premises scheme rule]
  (conj premises (a/pm (list 'valid (:id scheme)))))

(defn- apply-profile-to-scheme
  [profile scheme]
  (if-let [rule (first (filter #(= (:ruleid %) (:id scheme)) (:rules profile)))]
    (update-in scheme [:premises] insert-switch-rule scheme rule)
    scheme))

(defn- apply-profile-to-section
  [profile section]
  (assoc section :schemes
         (into [] (map (partial apply-profile-to-scheme profile) (:schemes section)))))

(defn extend-theory
  "Extend a theory to reflect the legal profile. Return the transformed theory."
  [theory profile]
  (tz/map-theory theory (partial apply-profile-to-section profile)))

(defn apply-legal-profile
  "Apply a legal profile to an argumentation graph."
  [g evaluator profile]
  (let [set-values (fn [g rules]
                     (reduce (fn [g {:keys [value ruleid]}]
                               (let [stmt (list 'valid ruleid)]
                                 (cond (in-value? value) (ag/accept g [stmt])
                                       (out-value? value) (ag/reject g [stmt])
                                       :else (ag/enter-statement g stmt))))
                             g
                             rules))] 
    (evaluate evaluator (set-values g (:rules profile)))))
