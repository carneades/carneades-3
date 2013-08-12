;;; Copyright (c) 2013 Fraunhofer Gesellschaft
;;; Licensed under the EUPL V.1.1

(ns ^{:doc ""}
  carneades.web.license-analysis.model.analysis
  (:use [clojure.tools.logging :only (info debug error)]
        [carneades.engine.utils :only [unserialize-atom]]
        [carneades.engine.dialog :only [add-answers]]
        [carneades.database.export :only [export-to-argument-graph]])
  (:require [clojure.pprint :as pp]
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
            [carneades.database.db :as db]))

(def markos-triplestore-endpoint "http://markos.man.poznan.pl/openrdf-sesame")
(def markos-repo-name "markos_test_sp2")
(def markos-namespaces [["top" "http://www.markosproject.eu/ontologies/top#"]
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

(defn- start-engine
  [params]
  (let [{:keys [project theories entity query repo-name endpoint ag-name]} params
        sexp (unserialize-atom query)
        loaded-theories (project/load-theory project theories)
        [argument-from-user-generator questions send-answer]
        (ask/make-argument-from-user-generator (fn [p] (questions/askable? loaded-theories p)))
        ag (get-ag project ag-name)
        engine (shell/make-engine ag 500 #{}
                                  ;; TODO: debug triplestore generator
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

(defn analyse
  "Begins an analysis of a given software entity. The theories inside project is used.
Returns a set of questions for the frontend."
  [params]
  (start-engine params))

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

(defn debug-query
  "Returns the result of query in the triplestore"
  [endpoint repo-name query limit]
  (let [conn (triplestore/make-conn endpoint
                                    repo-name
                                    markos-namespaces)
        sexp (unserialize-atom query)]
    (prn "sexp=" sexp)
    (try
      (binding [sparql/*select-limit* limit]
        {:result (pp/write (sparql/query (:kb conn) sexp)
                           :stream nil)})
      (catch Exception e
        {:result (.getMessage e)}))))

(defn debug-ask
  "Returns the ask result of query in the triplestore"
  [endpoint repo-name query limit]
  (let [conn (triplestore/make-conn endpoint
                                    repo-name
                                    markos-namespaces)
        sexp (unserialize-atom query)]
    (prn "sexp=" sexp)
    (try
      (binding [sparql/*select-limit* limit]
        {:result (pp/write (sparql/ask (:kb conn) sexp)
                           :stream nil)})
      (catch Exception e
        {:result (.getMessage e)}))))
