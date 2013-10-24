;;; Copyright (c) 2013 Fraunhofer Gesellschaft
;;; Licensed under the EUPL V.1.1

(ns ^{:doc "HTTP routes definitions for the license-analysis REST service."}
  carneades.web.license-analysis.routes.service
  (:use [compojure.core :only [defroutes GET POST]]
        [carneades.engine.utils :only [safe-read-string]])
  (:require [clojure.pprint :refer [pprint]]
            [carneades.engine.utils :refer [unserialize-atom]]
            [carneades.web.license-analysis.model.debug-analysis :as debug-analysis]
            [carneades.web.license-analysis.model.analysis :as analysis]
            [carneades.web.license-analysis.model.entity :as entity]))

(defroutes license-analysis-routes
  (POST "/analyse" {params :params}
        {:body (analysis/analyse params)})

  (GET "/software-entity/:project/" {{id :id
                                      project :project} :params}
       {:body (entity/get-software-entity project (unserialize-atom id))})

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; debug ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (POST "/debug/analyse" {params :params}
        {:body (debug-analysis/analyse params)})

  (POST "/send-answers" {{answers :answers
                          uuid :uuid} :params}
        {:body (analysis/process-answers answers uuid)})

  (POST "/debug/query" {{query :query
                         limit :limit
                         endpoint :endpoint
                         repo-name :repo-name} :params}
        {:body (debug-analysis/query endpoint repo-name query limit)})

  (POST "/debug/ask" {{query :query
                       limit :limit
                       endpoint :endpoint
                       repo-name :repo-name} :params}
        {:body (debug-analysis/ask endpoint repo-name query limit)}))
