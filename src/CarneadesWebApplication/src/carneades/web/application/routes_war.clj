(ns carneades.web.application.routes-war
  (:use carneades.web.application.routes)
  (:require [compojure.handler :as handler]))

(def app (handler/site carneades-application-routes))
