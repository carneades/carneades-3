;;; Copyright (c) 2012 Fraunhofer Gesellschaft
;;; Licensed under the EUPL V.1.1

(ns carneades.analysis.web.routes-war
  (:use carneades.policy-analysis.web.routes
        compojure.core
        [hiccup.middleware :only (wrap-base-url)]
        [ring.middleware.format-params :only [wrap-json-params]])
  (:require [compojure.handler :as handler]
            [ring.middleware.format-response :refer [wrap-restful-response]]
            [ring.middleware.stacktrace :refer [wrap-stacktrace]]
            [ring.middleware.keyword-params :refer [wrap-keyword-params]]
            [carneades.web.license-analysis.routes.service :refer [license-analysis-routes]]))

(defroutes carneades-webapp-routes
  policy-analysis-routes
  (context "/license-analysis" []
           (wrap-restful-response license-analysis-routes)))

(def carneades-webapp
  (-> carneades-webapp-routes
      (wrap-keyword-params)
      (wrap-json-params)
      (handler/site)
      (wrap-base-url)
      (wrap-stacktrace)))
