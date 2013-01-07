;;; Copyright (c) 2012 Fraunhofer Gesellschaft
;;; Licensed under the EUPL V.1.1

(ns impact.web.routes-dev
  (:use impact.web.routes
        carneades.web.service
        compojure.core
        impact.web.jetty
        ring.adapter.jetty
        ring.middleware.session
        ring.middleware.stacktrace
        [hiccup.middleware :only (wrap-base-url)]
        [ring.middleware.format-response :only [wrap-restful-response]])
  (:require [compojure.handler :as handler]))

(defroutes all-impact-pm-tool-routes
  (context "/impactws" [] (wrap-restful-response carneades-web-service-routes))
  (context "/policymodellingtool" [] impact-pm-tool-routes))

(def impact-app
  (-> (handler/site all-impact-pm-tool-routes)
      (wrap-base-url)))

;; (def impact-server nil)
;; (.start impact-server)
;; (.stop impact-server)
;; (defonce impact-server (run-jetty #'impact-app {:join? false :port 8080}))