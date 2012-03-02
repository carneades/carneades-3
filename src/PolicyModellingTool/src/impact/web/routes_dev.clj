(ns impact.web.routes-dev
  (:use carneades.web.service
        carneades.web.application.routes-lib
        compojure.core
        impact.web.jetty
        ring.adapter.jetty
        impact.web.views.pages
        impact.web.logic.server-properties
        ring.middleware.params
        ring.middleware.session
        [hiccup.middleware :only (wrap-base-url)])
  (:require [compojure.route :as route]
            [compojure.handler :as handler]
            [compojure.response :as response]
            [impact.web.controllers.policy-simulation :as simulation]
            [impact.web.controllers.policy-evaluation :as evaluation]
            [impact.web.controllers.svg :as svg]
            [impact.web.controllers.translation :as translation]))


(defroutes impact-pm-tool-routes
  
  (GET "/viewsession" {session :session} (str session))
  (GET "/resetsession" [] (simulation/reset-session))
  (GET "/" [] (simulation/init-page))
  (POST "/PolicySimulation"
        {session :session body :body params :params}
        (simulation/process-ajax-request session body params))
  (POST "/PolicyEvaluation"
        {session :session  body :body params :params}
        (evaluation/process-ajax-request session body params))
  (POST "/Translation"
        {session :session  body :body params :params}
        (translation/process-ajax-request session body params))
  (route/resources "/")
  ;; (route/not-found "Page not found")
  )

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

