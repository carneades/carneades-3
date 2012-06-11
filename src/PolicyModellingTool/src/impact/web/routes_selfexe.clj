(ns impact.web.routes-selfexe
  (:use impact.web.routes
        compojure.core
        ring.adapter.jetty
        [hiccup.middleware :only (wrap-base-url)])
  (:require [compojure.handler :as handler])
  (:gen-class))

(def impact-app
  (-> (handler/site impact-pm-tool-routes)
      (wrap-base-url)))

(defn start-server
  []
  (run-jetty #'impact-app {:join? false :port 8080}))

(defn -main
  [& args]
  (start-server))