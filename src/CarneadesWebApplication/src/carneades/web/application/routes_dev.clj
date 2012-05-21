(ns carneades.web.application.routes-dev
  (:use compojure.core
        carneades.web.application.routes
        ring.adapter.jetty
        carneades.web.service)
  (:require [compojure.route :as route]
            [compojure.handler :as handler]
            [compojure.response :as response]))

(defroutes all-carneades-application-routes
  (context "/impactws" [] carneades-web-service-routes)
  (context "/argumentbrowser" [] carneades-application-routes))

(def app (handler/site all-carneades-application-routes))

;; (defonce agb-server (run-jetty #'app {:join? false :port 8080}))

