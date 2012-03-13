(ns impact.web.routes-dev
  (:use impact.web.routes
        carneades.web.service
        carneades.web.application.routes
        compojure.core
        impact.web.jetty
        ring.adapter.jetty
        ring.middleware.session
        [hiccup.middleware :only (wrap-base-url)])
  (:require [compojure.handler :as handler]))

(defroutes all-impact-pm-tool-routes
  (context "/impactws" [] carneades-web-service-routes)
  (context "/argumentbrowser" [] carneades-application-routes)
  (context "/policymodellingtool" [] impact-pm-tool-routes))

(def impact-app
  (-> (handler/site all-impact-pm-tool-routes)
      (wrap-base-url)))

;; (defonce impact-server (run-jetty #'impact-app {:join? false :port 8080}))

;; (.start impact-server)
;; (.stop impact-server)
;; to comment when building the JAR:
;; (defonce impact-server (run-jetty-with-wars #'impact-app
;;                          {:join? false :port 8080}
;;                          [{:context-path "/argumentbrowser"
;;                            :war-path "../CarneadesWebApplication/carneades-web-application-1.0.0-SNAPSHOT-standalone.war"}
;;                           {:context-path "/impactws"
;;                            :war-path "../CarneadesWebService/carneades-web-service-1.0.0-SNAPSHOT-standalone.war"}]))

