(ns carneades.web.application.routes
  (:use compojure.core
        carneades.web.application.views.pages)
  (:require [compojure.route :as route]))

(defroutes carneades-application-routes
  (GET "/" [] (index-page))
  ;; (route/files "/" {:root (str (System/getProperty "user.dir") "/data/public")})
  (route/resources "/"  {:root "carneadeswebapplication/public"})
  )

