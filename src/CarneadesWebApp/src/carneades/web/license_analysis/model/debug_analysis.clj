;;; Copyright (c) 2013 Fraunhofer Gesellschaft
;;; Licensed under the EUPL V.1.1

(ns carneades.web.license-analysis.model.debug-analysis
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
            [carneades.database.db :as db]
            [carneades.web.license-analysis.model.analysis :refer :all]
            [carneades.engine.utils :refer [unserialize-atom]]))


(defn query
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

(defn ask
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

(defn analyse
  "Begins an analysis of a given software entity. The theories inside project is used.
Returns a set of questions for the frontend."
  [params]
  (start-engine params))
