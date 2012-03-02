(ns carneades.web.application.routes-war
  (:use compojure.core
        ring.middleware.params
        ring.middleware.session
        carneades.web.application.views.pages
        carneades.web.service)
  (:require [compojure.route :as route]
            [compojure.handler :as handler]
            [compojure.response :as response]))

(def carneades-application-routes
  [(GET "/" [] (index-page))
   (route/files "/" {:root (str (System/getProperty "user.dir") "/data/public")})
   (route/resources "/")])

(def allroutes
     carneades-application-routes)

(def app (handler/site (apply routes allroutes)))

