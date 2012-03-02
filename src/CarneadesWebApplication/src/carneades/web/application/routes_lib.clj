(ns carneades.web.application.routes-lib
  (:use compojure.core
        carneades.web.application.jetty
        ring.adapter.jetty
        ring.middleware.params
        ring.middleware.session
        carneades.web.application.views.pages
        carneades.web.service)
  (:require [compojure.route :as route]
            [compojure.handler :as handler]
            [compojure.response :as response]))

(defroutes carneades-application-routes
  (GET "/" [] (index-page))
  ;; (route/files "/" {:root (str (System/getProperty "user.dir") "/data/public")})
  (route/resources "/"  ;; {:root "/home/pal/Documents/Projects/carneades/src/CarneadesWebApplication"}
                   )
  )

(defroutes all-carneades-application-routes
  (context "/impactws" [] carneades-web-service-routes)
  (context "/argumentbrowser" [] carneades-application-routes))


(def app (handler/site all-carneades-application-routes))


;; (defonce server
;;   (run-jetty-with-wars #'app {:join? false :port 8080}
;;     [{:context-path "/impactws"
;;       :war-path "../CarneadesWebService/carneades-web-service-1.0.0-SNAPSHOT-standalone.war"}]
;;     ))

;; (defn -main [])

