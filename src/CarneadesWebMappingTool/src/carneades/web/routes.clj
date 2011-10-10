(ns carneades.web.routes
  (:use compojure.core
        carneades.web.views
        ring.adapter.jetty ;; <- to comment when building WAR
        [hiccup.middleware :only (wrap-base-url)]
        ring.middleware.session
        ring.middleware.params)
  (:require [compojure.route :as route]
            [compojure.handler :as handler]
            [compojure.response :as response]
            [ring.middleware [multipart-params :as mp]]))

(defroutes main-routes
  (GET "/" [] (index-page))
  (POST "/files" {params :params session :session}
        (upload-file (get params :lkif-file) session))
  (GET "/files" {params :params session :session} (view-file session params))
  (GET "/session" {session :session} (view-session session))
  (route/resources "/")
  (route/not-found "Page not found"))

(def app
  (-> (handler/site main-routes)
      (wrap-base-url)))

(defonce server (run-jetty #'app
                           {:join? false
                            :port 8080}))

