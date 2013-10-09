;;; Copyright (c) 2012 Fraunhofer Gesellschaft
;;; Licensed under the EUPL V.1.1

(ns carneades.analysis.web.routes-war
  (:use carneades.policy-analysis.web.routes
        compojure.core
        [hiccup.middleware :only (wrap-base-url)]
        [ring.middleware.json :only [wrap-json-params]])
  (:require [compojure.handler :as handler]
            [ring.middleware.format-response :refer [wrap-restful-response]]
            [ring.middleware.stacktrace :refer [wrap-stacktrace]]
            [ring.middleware.keyword-params :refer [wrap-keyword-params]]
            [carneades.web.license-analysis.routes.service :refer [license-analysis-routes]]
            [clj-logging-config.log4j :refer :all]))

(defroutes carneades-webapp-routes
  policy-analysis-routes
  (context "/license-analysis" []
           (-> license-analysis-routes
               (wrap-restful-response))))

(set-logger! :pattern "[%c]%n%m%n" :level :info)

(def carneades-webapp
  (-> carneades-webapp-routes
      (wrap-keyword-params)
      (wrap-json-params)
      (handler/site)
      (wrap-base-url)
      (wrap-stacktrace)))
