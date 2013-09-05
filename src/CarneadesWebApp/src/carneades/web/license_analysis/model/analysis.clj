;;; Copyright (c) 2013 Fraunhofer Gesellschaft
;;; Licensed under the EUPL V.1.1

(ns ^{:doc ""}
  carneades.web.license-analysis.model.analysis
  (:use [clojure.tools.logging :only (info debug error)]
        [carneades.engine.dialog :only [add-answers]]
        [carneades.database.export :only [export-to-argument-graph]])
  (:require [clojure.pprint :refer [pprint]]
            [carneades.engine.shell :as shell]
            [carneades.engine.theory :as theory]
            [carneades.engine.argument-graph :as ag]
            [carneades.engine.ask :as ask]
            [carneades.engine.dialog :as dialog]
            [carneades.project.admin :as project]
            [carneades.policy-analysis.web.logic.askengine :as policy]
            [carneades.policy-analysis.web.logic.questions :as questions]
            [carneades.engine.triplestore :as triplestore]
            [carneades.engine.uuid :as uuid]
            [edu.ucdenver.ccp.kr.sparql :as sparql]
            [carneades.policy-analysis.web.controllers.reconstruction :as recons]
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
            [carneades.engine.statement :as st]))

(def markos-triplestore-endpoint "http://markos.man.poznan.pl/openrdf-sesame")
(def markos-repo-name "markos_test_sp2")
(def markos-namespaces [["" "http://markosproject.eu/"]
                        ["top" "http://www.markosproject.eu/ontologies/top#"]
                        ["reif" "http://www.markosproject.eu/ontologies/reification#"]
                        ["soft" "http://www.markosproject.eu/ontologies/software#"]
                        ["lic" "http://www.markosproject.eu/ontologies/licenses#"]
                        ["kb" "http://markosproject.eu/kb/"]
                        ["package" "http://markosproject.eu/kb/Package/"]
                        ["directory" "http://markosproject.eu/kb/Directory/"]
                        ["api" "http://markosproject.eu/kb/API/"]
                        ["softwareproject" "http://markosproject.eu/kb/SoftwareProject/"]
                        ["softwarerelease" "http://markosproject.eu/kb/SoftwareRelease/"]
                        ["programminglanguage" "http://markosproject.eu/kb/ProgrammingLanguage/"]])

(defn initial-state
  []
  {:analyses {}})

(def state (atom (initial-state)))

(defn- index-analysis
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

(defn start-engine
  [params]
  (prn "params=")
  (prn params)
  (let [{:keys [project theories entity query repo-name endpoint ag-name]} params
        sexp (unserialize-atom query)
        loaded-theories (project/load-theory project theories)
        [argument-from-user-generator questions send-answer]
        (ask/make-argument-from-user-generator (fn [p] (questions/askable? loaded-theories p)))
        ag (get-ag project ag-name)
        engine (shell/make-engine ag 500 #{}
                                  (list
                                   (triplestore/generate-arguments-from-triplestore endpoint
                                                                                    repo-name
                                                                                    markos-namespaces)
                                   (theory/generate-arguments-from-theory loaded-theories)
                                   argument-from-user-generator))
        future-ag (future (shell/argue engine sexp))
        analysis {:ag nil
                  :project project
                  :uuid (symbol (uuid/make-uuid-str))
                  :lang :en
                  :query sexp
                  :policies loaded-theories
                  :future-ag future-ag
                  :questions questions
                  :send-answer send-answer
                  :dialog (dialog/make-dialog)
                  :last-id 0}
        analysis (policy/get-ag-or-next-question analysis)]
    (swap! state index-analysis analysis)
    (build-response analysis)))

(defn process-answers
  "Process the answers send by the user and returns new questions or an ag."
  [answers uuid]
  (prn "process answers...")
  (when-let [analysis (get-in @state [:analyses (symbol uuid)])]
    (let [{:keys [policies dialog]} analysis
          questions-to-answers (recons/reconstruct-answers answers
                                                           dialog
                                                           policies)
          analysis (update-in analysis [:dialog] add-answers questions-to-answers)
          ;; _ (prn "analysis after update")
          ;; _ (pp/pprint (:questions analysis))
          analysis (policy/send-answers-to-engine analysis)]
      (swap! state index-analysis analysis)
      (build-response analysis))))

(defn generate-arguments-from-mock-triplestore
  "Creates a generator generating arguments from facts in a triplestore.
Prefixes is a list of prefixes in the form (prefix namespace),
for instance (\"fn:\" \"http://www.w3.org/2005/xpath-functions#\") "
  ([endpoint-url repo-name prefixes]
     ;; TODO: check how to get all these information in the repository
     (reify generator/ArgumentGenerator
       (generate [this goal subs]
         (prn "[mock] goal=" goal)
         (let [[p s o] goal]
           (cond (= p 'http://www.markosproject.eu/ontologies/software#linkedLibrary)
                 (let [arg (argument/make-argument :conclusion goal
                                                   :scheme (str "mock-triplestore:x")
                                                   :strict true)
                       subs (assoc subs o 'MockSofwareEntity)]
                   (prn "[mock] returning response for linkedLibrary")
                   [(generator/make-response subs [] arg)])

                 (and (= p 'http://www.markosproject.eu/ontologies/oss-licenses#ReciprocalLicenseTemplate)
                      (not (st/variable? s)))
                 (let [arg (argument/make-argument :conclusion goal
                                                   :scheme (str "mock-triplestore:x")
                                                   :strict true)]
                   (prn "[mock] returning response for ReciprocalLicenseTemplate")
                   [(generator/make-response subs [] arg)])

                 (and (= p 'http://www.markosproject.eu/ontologies/copyright#licenseTemplate)
                      (not (st/variable? s)))
                 (let [arg (argument/make-argument :conclusion goal
                                                   :scheme (str "mock-triplestore:x")
                                                   :strict true)
                       subs (assoc subs o 'http://www.markosproject.eu/ontologies/oss-licenses#GPL-2.0)]
                   (prn "[mock] returning response for LicenseTemplate")
                   [(generator/make-response subs [] arg)])

                 ;; else
             :else ())))))
  ([endpoint-url]
     (generate-arguments-from-mock-triplestore endpoint-url "" [])))

(defn generate-arguments-from-mock-triplestore-transition
  "Creates a generator generating arguments from facts in a triplestore.
Prefixes is a list of prefixes in the form (prefix namespace),
for instance (\"fn:\" \"http://www.w3.org/2005/xpath-functions#\") "
  ([endpoint-url repo-name prefixes]
     (let [triplestore-generator (triplestore/generate-arguments-from-triplestore
                                  endpoint-url repo-name prefixes)]
      (reify generator/ArgumentGenerator
        (generate [this goal subs]
          (prn "[mock] goal=" goal)
          (let [[p s o] goal]
            (cond ;; (= p 'http://www.markosproject.eu/ontologies/software#linkedLibrary)
                  ;; (let [arg (argument/make-argument :conclusion goal
                  ;;                                   :scheme (str "mock-triplestore:x")
                  ;;                                   :strict true)
                  ;;       subs (assoc subs o 'MockSofwareEntity)]
                  ;;   (prn "[mock] returning response for linkedLibrary")
                  ;;   [(generator/make-response subs [] arg)])

                  ;; (and (= p 'http://www.markosproject.eu/ontologies/oss-licenses#ReciprocalLicenseTemplate)
                  ;;      (not (st/variable? s)))
                  ;; (let [arg (argument/make-argument :conclusion goal
                  ;;                                   :scheme (str "mock-triplestore:x")
                  ;;                                   :strict true)]
                  ;;   (prn "[mock] returning response for ReciprocalLicenseTemplate")
                  ;;   [(generator/make-response subs [] arg)])

                  ;; (and (= p 'http://www.markosproject.eu/ontologies/copyright#licenseTemplate)
                  ;;      (not (st/variable? s)))
                  ;; (let [arg (argument/make-argument :conclusion goal
                  ;;                                   :scheme (str "mock-triplestore:x")
                  ;;                                   :strict true)
                  ;;       subs (assoc subs o 'http://www.markosproject.eu/ontologies/oss-licenses#GPL-2.0)]
                  ;;   (prn "[mock] returning response for LicenseTemplate")
                  ;;   [(generator/make-response subs [] arg)])

                  ;; else
                  :else (generator/generate triplestore-generator goal subs)))))))
  ([endpoint-url]
     (generate-arguments-from-mock-triplestore endpoint-url "" [])))

(defonce ag-nb (atom 500))

(defn inc-ag-number!
  []
  (swap! ag-nb inc))

(defn scratch-start-engine-transition
  []
  ;; http://markosproject.eu/kb/SoftwareRelease/_2
  (let [query "(http://www.markosproject.eu/ontologies/copyright#mayBeLicensedUsing
               http://markosproject.eu/kb/SoftwareRelease/_365
               http://www.markosproject.eu/ontologies/oss-licenses#BSD-2.0-Clause)"
        sexp (unserialize-atom query)
        _ (prn "sexp=" sexp)
        project "markos"
        theories "oss_licensing_theory"
        endpoint "http://markos.man.poznan.pl/openrdf-sesame"
        repo-name "markos_test_26-07-2013"
        loaded-theories (project/load-theory project theories)
        [argument-from-user-generator questions send-answer]
        (ask/make-argument-from-user-generator (fn [p] (questions/askable? loaded-theories p)))
        ag (ag/make-argument-graph) ;; (get-ag project ag-name)
        engine (shell/make-engine ag 3000 #{}
                                  (list
                                   (generate-arguments-from-mock-triplestore-transition endpoint
                                                                                        repo-name
                                                                                        markos-namespaces)
                                   (theory/generate-arguments-from-theory loaded-theories)
                                   argument-from-user-generator))
        ag (shell/argue engine sexp)
        ag (agr/enter-language ag (:language loaded-theories))
        agnumber (inc-ag-number!)
        dbname (str "ag" (str agnumber))]
    (ag-db/create-argument-database "markos" dbname "root" "pw1" (dc/make-metadata))
    (import-from-argument-graph (db/make-connection "markos" dbname "root" "pw1") ag true)
    ;; (lacij/export ag "/tmp/ag1.svg")

    ;; (prn "ag =")
    ;; (pprint ag)
    (prn "nb statements=" (count (:statement-nodes ag)))
    (prn "AG NUMBER = " agnumber)))

(defn scratch-start-engine
  []
  (let [query "(http://www.markosproject.eu/ontologies/copyright#mayBeLicensedUsing
               http://markosproject.eu/kb/Package/abc42_3
               http://www.markosproject.eu/ontologies/oss-licenses#BSD-2.0-Clause)"
        sexp (unserialize-atom query)
        _ (prn "sexp=" sexp)
        project "markos"
        theories "oss_licensing_theory"
        endpoint "http://markos.man.poznan.pl/openrdf-sesame"
        repo-name "markos_test_sp2"
        loaded-theories (project/load-theory project theories)
        [argument-from-user-generator questions send-answer]
        (ask/make-argument-from-user-generator (fn [p] (questions/askable? loaded-theories p)))
        ag (ag/make-argument-graph) ;; (get-ag project ag-name)
        engine (shell/make-engine ag 1500 #{}
                                  (list
                                   (generate-arguments-from-mock-triplestore endpoint
                                                                        repo-name
                                                                        markos-namespaces)
                                   (theory/generate-arguments-from-theory loaded-theories)
                                   argument-from-user-generator))
        ag (shell/argue engine sexp)
        ag (agr/enter-language ag (:language loaded-theories))
        agnumber (inc-ag-number!)
        dbname (str "ag" (str agnumber))]
    (ag-db/create-argument-database "markos" dbname "root" "pw1" (dc/make-metadata))
    (import-from-argument-graph (db/make-connection "markos" dbname "root" "pw1") ag true)
    (lacij/export ag "/tmp/ag1.svg")
    (prn "nb statements=" (count (:statement-nodes ag)))
    ;; (prn "ag =")
    ;; (pprint ag)
    (prn "AG NUMBER = " agnumber)))

(defn test-generate-arguments-from-triplestore-concept
  []
  (let [endpoint "http://markos.man.poznan.pl/openrdf-sesame"
        repo-name "markos_test_26-07-2013"
        ;; goal2 '(http://www.markosproject.eu/ontologies/oss-licenses#ReciprocalLicenseTemplate ?x)
        goal '(http://www.markosproject.eu/ontologies//oss-licenses#Apache-2.0 rdf/type http://www.markosproject.eu/ontologies//oss-licenses#ReciprocalLicenseTemplate)
        subs {}
        triplestore-generator (triplestore/generate-arguments-from-triplestore
                               endpoint repo-name markos-namespaces)
        responses (generator/generate triplestore-generator goal subs)]
    (pprint responses)))
