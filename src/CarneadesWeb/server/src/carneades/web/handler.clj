;; Copyright (c) 2014 Fraunhofer Gesellschaft
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns carneades.web.handler
  ^{:author "Sebastian Kaiser"
    :doc "Handler for servlet container. Initiates routes."}
  (:require [compojure.core :refer :all]
            [ring.middleware.json :refer [wrap-json-response]]
            [noir.util.middleware :as middleware]
            [sandbar.stateful-session :refer :all]
            [carneades.web.routes :refer [carneades-web-routes]]
            [compojure.route :as route :refer [files resources not-found]]
            [sandbar.stateful-session :as session]))

(defroutes app-routes
  (route/files "/carneades" {:root "../client/dist"})
  (route/resources "/carneades")
  (route/not-found "Not Found"))

;;append your application routes to the all-routes vector
(def all-routes [carneades-web-routes app-routes])

(defn init
  "init will be called once when
   app is deployed as a servlet on
   an app server such as Tomcat
   put any initialization code here"
  []
  ;(create-tables)
  (println "carneades started successfully..."))

(defn destroy
  "destroy will be called when your application
   shuts down, put any clean up code here"
  []
  (println "shutting down..."))

(def app (-> all-routes
             middleware/app-handler
             session/wrap-stateful-session
             ;(wrap-keyword-params)
             ;(wrap-json-params)
             ;(wrap-json-response)
             ;;add your middlewares here
             ))

(def war-handler (middleware/war-handler app))
