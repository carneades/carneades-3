(ns carneades.web.routes
  (:use compojure.core
        carneades.web.views
        [hiccup.middleware :only (wrap-base-url)]
        ring.adapter.jetty
        ring.middleware.session
        ring.middleware.keyword-params
        ring.middleware.params)
  (:require [compojure.route :as route]
            [compojure.handler :as handler]
            [compojure.response :as response]
            [ring.middleware [multipart-params :as mp]]))

(defroutes main-routes
  (GET "/" [] (index-page))
  (mp/wrap-multipart-params
   (POST "/files" {params :params session :session}
         (upload-file (get params "lkif-file") session)))
  (wrap-params
   (GET "/files" {params :params session :session} (view-file session params)))
  (GET "/session" {session :session} (view-session session))
  (route/resources "/")
  (route/not-found "Page not found"))

(wrap! main-routes :session)

(def app
  (-> (handler/site main-routes)
      ;; (wrap-keyword-params) not working?
      (wrap-base-url)))

(defonce server (run-jetty #'main-routes
                           {:join? false
                            :port 8080}))

