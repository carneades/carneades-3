(ns carneades.web.application.routes
  (:use compojure.core
;; ring.adapter.jetty ;; <- to comment when building WAR
        ring.middleware.params
        ring.middleware.session
        carneades.web.application.views.pages
        carneades.web.service)
  (:require [compojure.route :as route]
            [compojure.handler :as handler]
            [compojure.response :as response])
  ;; comment this when building WAR:
  ;; (:gen-class)
  )

(def carneades-application-routes
     [(GET "/" [] (index-page))
      (route/files "/" {:root (str (System/getProperty "user.dir") "/data/public")})
      (route/resources "/")
      ])

(def allroutes
     ;; carneades-application-routes
     ;; use the following combination of routes to embed the Web Service within the WebApplication 
     (concat carneades-application-routes carneades-web-service-routes)
     )
(def app (handler/site (apply routes allroutes)))

(defn start-server
  []
  ;; comment this when building WAR:
  ;; (run-jetty #'app {:join? false :port 8080})
  )

(defn -main
  [& args]
  (start-server))
