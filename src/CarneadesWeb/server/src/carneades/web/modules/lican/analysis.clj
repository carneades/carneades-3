;; Copyright (c) 2013 Fraunhofer Gesellschaft
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns ^{:doc "Analysis of licenses."}
  carneades.web.modules.lican.analysis
  (:use [carneades.engine.dialog :only [add-answers]]
        [carneades.database.export :only [export-to-argument-graph]])
  (:require [clojure.pprint :refer [pprint]]
            [carneades.engine.shell :as shell]
            [carneades.engine.theory :as theory]
            [carneades.engine.argument-graph :as ag]
            [carneades.engine.ask :as ask]
            [carneades.engine.dialog :as dialog]
            [carneades.project.admin :as project]
            [carneades.web.modules.common.dialog.engine :as engine]
            [carneades.web.modules.common.dialog.questions :as questions]
            [carneades.engine.triplestore :as triplestore]
            [carneades.engine.uuid :as uuid]
            [edu.ucdenver.ccp.kr.sparql :as sparql]
            [carneades.web.modules.common.dialog.reconstruction :as recons]
            [carneades.engine.triplestore :as triplestore]
            [carneades.database.db :as db]
            [carneades.database.argument-graph :as ag-db]
            [carneades.engine.utils :refer [unserialize-atom]]
            [carneades.maps.lacij :as lacij]
            [carneades.engine.argument-generator :as generator]
            [carneades.engine.argument :as argument]
            [carneades.database.import :refer [import-from-argument-graph]]
            [carneades.engine.dublin-core :as dc]
            [carneades.engine.argument-graph :as agr]
            [carneades.engine.statement :as st]
            [carneades.engine.argument-evaluation :as evaluation]
            [carneades.engine.caes :refer [caes]]
            [carneades.engine.theory.namespace :as namespace]
            [carneades.web.modules.lican.triplestore :as tp]
            [carneades.engine.translation :as tr]
            [carneades.engine.theory.translation :as ttr]
            [carneades.web.modules.lican.entity :as entity]
            [taoensso.timbre :as timbre :refer [debug spy info warn error]]
            [carneades.database.legal-profile :as lp]
            [carneades.engine.legal-profile :refer [extend-theory
                                                    empty-legal-profile]]
            [carneades.engine.aspic :refer [aspic-grounded]]
            [carneades.web.modules.common.dialog.utils :refer [store-ag]]

            [carneades.engine.unify :refer [unify]]))

(defn initial-state
  []
  {:analyses {}})

(def state (atom (initial-state)))

(defn index-analysis
  [state analysis]
  (assoc-in state [:analyses (:uuid analysis)] analysis))

(defn build-response
  [analysis]
  (if (:all-questions-answered analysis)
    {:db (:db analysis)
     :uuid (:uuid analysis)}
    {:questions (:last-questions analysis)
     :uuid (:uuid analysis)}))

(defn get-ag
  [project ag-name]
  (if (empty? ag-name)
    (ag/make-argument-graph)
    (let [dbconn (db/make-connection project ag-name "guest" "")
          ag (export-to-argument-graph dbconn)]
      ag)))

(defn process-answers
  "Process the answers send by the user and returns new questions or an ag."
  [answers uuid]
  (prn "process answers...")
  (when-let [analysis (get-in @state [:analyses (symbol uuid)])]
    (let [{:keys [policies dialog]} analysis
          _ (prn "[process-answers]")
          _ (prn "answers=" answers)
          questions-to-answers (recons/reconstruct-answers answers
                                                           dialog
                                                           policies)
          _ (prn "[:answers] questions-to-answers =" questions-to-answers)
          analysis (update-in analysis [:dialog] add-answers questions-to-answers)
          _ (prn "update-in finished")
          analysis (engine/send-answers-to-engine analysis)
          _ (prn "sending answers finished")]
      (swap! state index-analysis analysis)
      (let [r (build-response analysis)]
        (prn "response =")
        (prn r)
        r))))

(defonce ag-nb (atom 100))

(defn inc-ag-number!
  []
  (swap! ag-nb inc))

(defn compatible-license-test
  []
  ;; http://markosproject.eu/kb/SoftwareRelease/_2
  (let [query "(http://www.markosproject.eu/ontologies/oss-licenses#permissibleUse
               (http://www.markosproject.eu/ontologies/oss-licenses#use4
                http://www.markosproject.eu/ontologies/software#linkedLibrary
                http://markosproject.eu/kb/SoftwareRelease/366
                http://www.markosproject.eu/ontologies/software#Library
                http://markosproject.eu/kb/Library/1)
               )"
        sexp (unserialize-atom query)
        _ (prn "sexp=" sexp)
        project "markos"
        theories "oss_licensing_theory"
        endpoint "http://markos.man.poznan.pl/openrdf-sesame"
        repo-name "markos_test_26-07-2013"
        loaded-theories (project/load-theory project theories)
        [argument-from-user-generator questions send-answer]
        (ask/make-argument-from-user-generator (fn [p] (questions/askable? loaded-theories p)))
        ag (ag/make-argument-graph)
        properties (project/load-project-properties project)
        theories (:policies properties)
        triplestore (:triplestore properties)
        repo-name (:repo-name properties)
        markos-namespaces (:namespaces properties)
        engine (shell/make-engine ag 3000 #{}
                                  (list
                                   (triplestore/generate-arguments-from-triplestore endpoint
                                                                                    repo-name
                                                                                    markos-namespaces)
                                   (theory/generate-arguments-from-theory loaded-theories)
                                   argument-from-user-generator))
        ag (shell/argue engine sexp)
        ag (evaluation/evaluate caes ag)
        ag (ag/set-main-issues ag sexp)
        ;; TODO: ag (agr/enter-language ag (:language loaded-theories) markos-namespaces)
        agnumber (inc-ag-number!)
        dbname (str "ag" (str agnumber))]
    ;; (pprint ag)
    (ag-db/create-argument-database "markos" dbname "root" "pw1" (dc/make-metadata))
    (import-from-argument-graph (db/make-connection "markos" dbname "root" "pw1") ag true)
    (lacij/export ag "/tmp/ag1.svg")
    (prn "nb statements=" (count (:statement-nodes ag)))
    (prn "AG NUMBER = " agnumber)))

(defn license-analyzer-test
  []
  (let [query "(
               )"
        sexp (unserialize-atom query)
        project "markos"
        theories "oss_licensing_theory"
        endpoint "http://markos.man.poznan.pl/openrdf-sesame"
        repo-name "markos_test_15_04_2014"
        loaded-theories (project/load-theory project theories)
        [argument-from-user-generator questions send-answer]
        (ask/make-argument-from-user-generator (fn [p] (questions/askable? loaded-theories p)))
        ag (ag/make-argument-graph)
        properties (project/load-project-properties project)
        theories (:policies properties)
        triplestore (:triplestore properties)
        repo-name (:repo-name properties)
        markos-namespaces (:namespaces properties)
        engine (shell/make-engine ag 3000 #{}
                                  (list
                                   (triplestore/generate-arguments-from-triplestore endpoint
                                                                                    repo-name
                                                                                    markos-namespaces)
                                   (theory/generate-arguments-from-theory loaded-theories)
                                   argument-from-user-generator))
        ag (shell/argue engine sexp)
        ag (evaluation/evaluate caes ag)
        ag (ag/set-main-issues ag sexp)
        ;; TODO: ag (agr/enter-language ag (:language loaded-theories) markos-namespaces)
        agnumber (inc-ag-number!)
        dbname (str "ag" (str agnumber))]
    ;; (pprint ag)
    (ag-db/create-argument-database "markos" dbname "root" "pw1" (dc/make-metadata))
    (import-from-argument-graph (db/make-connection "markos" dbname "root" "pw1") ag true)
    (lacij/export ag "/tmp/ag1.svg")
    (prn "nb statements=" (count (:statement-nodes ag)))
    (prn "AG NUMBER = " agnumber)))

(defn on-ag-built
  [entity ag]
  (assoc ag :header (dc/make-metadata
                     :title (str "Analysis of the software release " (:name entity))
                     :description {:en (format "[Go back to %s page](http://demo.markosproject.eu/#t4&p12=%s)" (:name entity) (:uri entity))})))

(defn load-profile
  [project id]
  (if (or (empty? id) (= id "null"))
    empty-legal-profile
    (lp/with-db project "root" "pw1"
      (lp/read-profile+ id))))

(defn make-translator
  [triplestore repo-name theories namespaces]
  (comp (tr/make-default-translator)
        (ttr/make-language-translator theories)
        (ttr/make-uri-shortening-translator namespaces)
        (tp/make-uri-translator triplestore repo-name namespaces)))

(defn start-engine
  [entity legalprofileid]
  (let [project "markos"
        properties (project/load-project-properties project)
        triplestore (:triplestore properties)
        repo-name (:repo-name properties)
        markos-namespaces (:namespaces properties)
        licenses (tp/get-licenses (unserialize-atom entity)
                                  triplestore
                                  repo-name
                                  markos-namespaces)
        licenses-statements (map #(unserialize-atom
                                   (format "(http://www.markosproject.eu/ontologies/copyright#mayBeLicensedUsing %s %s)"
                                           entity
                                           %))
                                 licenses)
        theories (:policies properties)
        query (first licenses-statements)
        _ (info "licenses-statements: " licenses-statements)
        loaded-theories (project/load-theory project theories)
        translator (make-translator triplestore
                                    repo-name
                                    (:language loaded-theories)
                                    markos-namespaces)
        [argument-from-user-generator questions send-answer]
        (ask/make-argument-from-user-generator (fn [p] (questions/askable? loaded-theories p)))
        ag (get-ag project "")
        triplestore-generator (triplestore/generate-arguments-from-triplestore triplestore
                                                                               repo-name
                                                                               markos-namespaces)
        profile (load-profile project legalprofileid)
        _ (debug "profile " profile)
        loaded-theories' (extend-theory loaded-theories profile)
        engine (shell/make-engine+ ag 500 #{}
                                   (list
                                    triplestore-generator
                                    (theory/generate-arguments-from-theory loaded-theories')
                                    argument-from-user-generator))
        future-ag (future (shell/argue+
                           engine aspic-grounded licenses-statements profile))
        entity (entity/get-software-entity project (unserialize-atom entity))
        analysis {:ag nil
                  :project project
                  :uuid (symbol (uuid/make-uuid-str))
                  :lang :en
                  :query query
                  :policies loaded-theories
                  :future-ag future-ag
                  :questions questions
                  :send-answer send-answer
                  :dialog (dialog/make-dialog)
                  :last-id 0
                  :namespaces markos-namespaces
                  :translator translator
                  :post-build (partial on-ag-built entity)
                  }
        analysis (engine/get-ag-or-next-question analysis)]
    (swap! state index-analysis analysis)
    (build-response analysis)))

(defn analyse
  "Begins an analysis of a given software entity. The theories inside project is used.
Returns a set of questions for the frontend."
  [entity legalprofile]
  (info "[analyse]")
  (start-engine entity legalprofile))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; find compatible licenses

(defn- get-in-main-issues
  [ag]
  (filter (every-pred evaluation/in-node? :main) (vals (:statement-nodes ag))))

(defn- get-compatible-licenses
  [query in-main-issues]
  (mapcat (comp vals (partial unify query)) (map :atom in-main-issues)))

(defn find-compatible-licenses
  "Returns a list of Open Source license templates (e.g. GPL, BSD)
  which are compatible with the licenses of the software entities used
  by s, using the quick and dirty procedure to perform the license
  compatibility analysis."
  [legal-profile software-entity]
  (let [query-string (format "(http://www.markosproject.eu/ontologies/copyright#mayBeLicensedUsing %s ?x)" software-entity)
        project "markos"
        properties (project/load-project-properties project)
        theories (:policies properties)
        triplestore (:triplestore properties)
        repo-name (:repo-name properties)
        markos-namespaces (:namespaces properties)
        query (unserialize-atom query-string)
        loaded-theories (project/load-theory project theories)
        ag (ag/make-argument-graph)
        engine (shell/make-engine ag 500 #{}
                                  (list
                                   (triplestore/generate-arguments-from-triplestore triplestore
                                                                                    repo-name
                                                                                    markos-namespaces)
                                   (theory/generate-arguments-from-theory loaded-theories)))
        ag (shell/argue engine query)
        ag (evaluation/evaluate aspic-grounded ag)
        ag (ag/set-main-issues ag query)
        in-main-issues (get-in-main-issues ag)]
    (into [] (map str (get-compatible-licenses query in-main-issues)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; find software entities with compatible licenses

(defn goal-in?
  [goal the-entity use-property-uris generators profile sw-entity-uri]
  (let [g (ag/make-argument-graph)
        use-facts (map (fn [usage]
                         (unserialize-atom (str "(" usage " " the-entity " " sw-entity-uri ")")))
                       use-property-uris)
        engine (shell/make-engine g 500 use-facts generators)
        g (shell/argue engine aspic-grounded goal profile)
        goal-node (ag/get-statement-node g goal)]
    (evaluation/in-node? goal-node)))

(defn filter-software-entities
  [goal the-entity use-property-uris sw-entity-uris generators profile]
  (filter (partial goal-in? goal the-entity use-property-uris generators profile) sw-entity-uris))

(defn find-software-entities-with-compatible-licenses
  ;; Here's a sketch of a procedure for implementing 0000516: Find software
  ;; entities with compatible licenses.

  ;; The strings are the URIs of the entities.  The strings returned are a
  ;; subset of these URIs, for the entities with compatible licenses.

  ;; 1. Create an argument graph, g1, with the main issue:
  ;; (http://www.markosproject.eu/ontologies/copyright#mayBeLicensedUsing
  ;;  theSoftwareEntity template), where theSoftwareEntity is a skolem
  ;; constant representing the existence of some software entity of interest.

  ;; (exists x (mayBeLicensedUsing x template)) => (mayBeLicensedUsing
  ;;                                                theSoftwareEntity template)

  ;; 2. Filter the entities, using this predicate function:

  ;; 2.1 let g2 = g1 +  facts (accepted statements) for each use of e1.
  ;; For example:

  ;; let uses = [u1, u2]

  ;; g2 = g1 + (u1 theSoftwareEntity e1) + (u2 theSoftwareEntity e1)

  ;; 2.2. Run the inference engine to construct arguments using the
  ;; copyright theory and the MARKOS repository, with the main issue as the
  ;; query, and g2 as the initial argument graph

  ;; 2.3 If the main issue is "in" (value = 1.0) then the argument graph
  ;; returned by evaluating g2 is OK and the filter function should return
  ;; true.  Otherwise false.
  [legal-profile-id license-template-uri use-property-uris sw-entity-uris]
  (let [project "markos"
        properties (project/load-project-properties project)
        triplestore (:triplestore properties)
        repo-name (:repo-name properties)
        markos-namespaces (:namespaces properties)
        theories (:policies properties)
        loaded-theories (project/load-theory project theories)
        the-entity "http://www.markosproject.eu/ontologies/software#theSoftware"
        goal (unserialize-atom
              (format (str "(http://www.markosproject.eu/ontologies/copyright#mayBeLicensedUsing"
                           " " the-entity
                           " %s)")
                      license-template-uri))
        _ (debug "goal: " goal)
        translator (make-translator triplestore
                                    repo-name
                                    (:language loaded-theories)
                                    markos-namespaces)
        profile (load-profile project legal-profile-id)
        loaded-theories' (extend-theory loaded-theories profile)
        triplestore-generator (triplestore/generate-arguments-from-triplestore
                               triplestore
                               repo-name
                               markos-namespaces)
        generators (list
                    triplestore-generator
                    (theory/generate-arguments-from-theory loaded-theories'))]
    (filter-software-entities goal the-entity use-property-uris sw-entity-uris generators profile)))


