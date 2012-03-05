(ns impact.web.routes-war
  (:use impact.web.routes
        compojure.core
        [hiccup.middleware :only (wrap-base-url)])
  (:require [compojure.handler :as handler]))

(def impact-app
  (-> (handler/site impact-pm-tool-routes)
      (wrap-base-url)))
