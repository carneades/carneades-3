(ns carneades.web.modules.admin.routes
  (:use compojure.core)
  (:require [carneades.web.modules.admin.views.layout :as layout]))

(defn admin-project-page []
  (layout/render "home.html" {:projects "abc"
                              :base "/carneades"
                              :context2 "/carneades/projects"}))

(defroutes admin-routes*
  (GET "/projects" [] (admin-project-page)))

(def admin-routes (-> #'admin-routes*))

(defroutes admin-web-routes
  (ANY "*" [] (context "/admin" [] admin-routes)))
