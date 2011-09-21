(ns impact.web.routes
  (:use compojure.core
        impact.web.views
        impact.web.controllers
        ring.adapter.jetty ;; <- to remove when building WAR
        ring.middleware.params
        ring.middleware.session
        [hiccup.middleware :only (wrap-base-url)])
  (:require [compojure.route :as route]
            [compojure.handler :as handler]
            [compojure.response :as response]))

(defroutes main-routes
  (GET "/" [] (init-page))
  (GET "/resetsession" [] (reset-session))
  (wrap-params
   (POST "/PolicySimulation"
         {session :session params :params}
         (process-ajax-request session params)))
  (route/resources "/")
  (route/not-found "Page not found"))

(wrap! main-routes :session)

(def app
  (-> (handler/site main-routes)
      (wrap-base-url)))

(defonce server (run-jetty #'main-routes
                           {:join? false
                            :port 8080}))

