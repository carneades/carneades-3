(ns impact.web.routes
  (:use compojure.core
        impact.web.views
        ring.adapter.jetty ;; <- to comment when building WAR
        ring.middleware.params
        ring.middleware.session
        [hiccup.middleware :only (wrap-base-url)])
  (:require [compojure.route :as route]
            [compojure.handler :as handler]
            [compojure.response :as response]
            [impact.web.policy-simulation :as simulation]
            [impact.web.policy-evaluation :as evaluation]
            [impact.web.svg :as svg]))

(defroutes main-routes
  (GET "/" [] (simulation/init-page))
  (GET "/viewsession" {session :session} (str session))
  (GET "/resetsession" [] (simulation/reset-session))
  (POST "/PolicySimulation"
        {session :session params :params}
        (simulation/process-ajax-request session params))
  (POST "/PolicyEvaluation"
        {session :session params :params}
        (evaluation/process-ajax-request session params))
  (POST "/svg/*"
        {uri :uri session :session  params :params}
        (svg/process-ajax-request uri session params))
  (route/resources "/")
  (route/not-found "Page not found"))

(def app
  (-> (handler/site main-routes)
      (wrap-base-url)))

;; to comment when building the JAR:
(defonce server (run-jetty #'app {:join? false :port 8080}))


