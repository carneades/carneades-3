(ns impact.web.routes
  (:use compojure.core
        impact.web.views.pages
        impact.web.logic.server-properties
 ring.adapter.jetty ;; <- to comment when building WAR
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

(defroutes main-routes
  ;; debugging
  (GET "/viewsession" {session :session} (str (dissoc (:service-data session)
                                                      :to-engine
                                                      :from-engine)))
  (GET "/resetsession" [] (simulation/reset-session))
  ;; (GET "/info" [] (server-properties))

  ;; production
  (GET "/" [] (simulation/init-page))
  (POST "/PolicySimulation"
        {session :session params :params}
        (simulation/process-ajax-request session params))
  (POST "/PolicyEvaluation"
        {session :session params :params}
        (evaluation/process-ajax-request session params))
  (POST "/svg/*"
        {uri :uri session :session  params :params}
        (svg/process-ajax-request uri session params))
  (POST "/Translation"
        {session :session params :params}
        (translation/process-ajax-request session params))
  (route/resources "/")
  (route/not-found "Page not found"))

(def app
  (-> (handler/site main-routes)
      (wrap-base-url)))

;; to comment when building the JAR:
;;  (defonce server (run-jetty #'app {:join? false :port 8080}))
