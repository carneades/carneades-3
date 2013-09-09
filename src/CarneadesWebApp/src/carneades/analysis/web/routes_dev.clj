;;; Copyright (c) 2012-2013 Fraunhofer Gesellschaft
;;; Licensed under the EUPL V.1.1

(ns carneades.analysis.web.routes-dev
  (:use carneades.policy-analysis.web.routes
        carneades.web.service
        compojure.core
        carneades.policy-analysis.web.jetty
        ring.adapter.jetty
        ring.middleware.session
        ring.middleware.stacktrace
        [hiccup.middleware :only (wrap-base-url)]
        [ring.middleware.format-response :only [wrap-restful-response]]
        [ring.middleware.json :only [wrap-json-params]]
        [ring.middleware.keyword-params :only [wrap-keyword-params]]
        [ring.middleware.stacktrace :only [wrap-stacktrace]]
        [carneades.web.license-analysis.routes.service :only [license-analysis-routes]])
  (:require [compojure.route :as route]
            [compojure.handler :as handler]))

(defroutes carneades-webapp-routes
  (context "/carneades" [] policy-analysis-routes)
  (context "/carneades/license-analysis" [] (wrap-restful-response license-analysis-routes))
  (context "/carneadesws" [] (wrap-restful-response carneades-web-service-routes)))

(def carneades-webapp
  (-> carneades-webapp-routes
      (wrap-keyword-params)
      (wrap-json-params)
      (handler/site)
      (wrap-base-url)
      ;; (wrap-stacktrace)
      ))

;; (def impact-server nil)
;; (.start impact-server)
;; (.stop impact-server)
;; (defonce impact-server (run-jetty #'carneades-webapp {:join? false :port 8080}))
