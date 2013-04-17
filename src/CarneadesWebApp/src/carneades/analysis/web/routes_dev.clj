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
        [ring.middleware.format-response :only [wrap-restful-response]])
  (:require [compojure.route :as route]
            [compojure.handler :as handler]))

(defroutes carneades-webapp-routes
  (context "/carneades/policy-analysis" [] policy-analysis-routes)
  ;; TODO: (context "/carneades/license-analysis" [] license-analysis-routes)
  (context "/carneadesws" [] (wrap-restful-response carneades-web-service-routes)))

(def carneades-webapp
  (-> (handler/site carneades-webapp-routes)
      (wrap-base-url)))

;; (def impact-server nil)
;; (.start impact-server)
;; (.stop impact-server)
;; (defonce impact-server (run-jetty #'carneades-webapp {:join? false :port 8080}))
