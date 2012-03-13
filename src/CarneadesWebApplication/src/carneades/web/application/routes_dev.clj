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

;; (defonce server
;;   (run-jetty-with-wars #'app {:join? false :port 8080}
;;     [{:context-path "/impactws"
;;       :war-path "../CarneadesWebService/carneades-web-service-1.0.0-SNAPSHOT-standalone.war"}]
;;     ))

;; (defn -main [])

