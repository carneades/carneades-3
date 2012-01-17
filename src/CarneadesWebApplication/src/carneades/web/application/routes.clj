(ns carneades.web.application.routes
  (:use compojure.core
        ring.adapter.jetty ;; <- to comment when building WAR
        ring.middleware.params
        ring.middleware.session
        carneades.web.application.views.pages
        carneades.web.service)
  (:require [compojure.route :as route]
            [compojure.handler :as handler]
            [compojure.response :as response])
  (:gen-class))

(def carneades-application-routes
     [(GET "/" [] (index-page))
      (route/files "/" {:root (str (System/getProperty "user.dir") "/webdata/public")})
      (route/resources "/")
      ])

(def allroutes (concat carneades-application-routes carneades-web-service-routes))
(def app (handler/site (apply routes allroutes)))

(defn start-server
  []
  (run-jetty #'app {:join? false :port 8080}))

(defn -main
  [& args]
  (start-server))
