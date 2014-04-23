;; Copyright (c) 2014 Fraunhofer Gesellschaft
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns carneades.web.handler
  ^{:doc "Handler for servlet container. Initiates routes."}
  (:require [clojure.string :as str]
            [taoensso.timbre :as timbre :refer (trace debug info warn error fatal spy)]
            [compojure.core :refer :all]
            [org.httpkit.server :refer [run-server]]
            [ring.server.standalone :refer [serve]]
            [noir.util.middleware :as middleware]
            [carneades.web.routes :refer [carneades-web-routes tomcat-carneades-web-routes]]
            [compojure.route :as route :refer [files resources not-found]]
            [sandbar.stateful-session :as session]
            [carneades.web.service :as service]
            [ring.middleware.params :refer [wrap-params]]
            [ring.middleware.multipart-params :refer [wrap-multipart-params]]
            [ring.middleware.keyword-params :refer [wrap-keyword-params]]
            [ring.util.codec :as codec]
            [ring.util.response :as response]
            [com.postspectacular.rotor :as rotor]))

(defroutes app-routes
  (route/resources "/")
  (route/not-found "Not Found"))

(defroutes tomcat-app-routes
  (route/resources "/" {:root "public/carneades"})
  (route/not-found "Not Found"))

(def logger-config
  {:appenders {:rotor {:min-level :info
                      :enabled? true
                      :async? false
                      :max-message-per-msecs nil
                      :fn rotor/append}
              :standard-out {:min-level :info
                             :enabled? true
                             :async? false
                             :max-message-per-msecs nil}}
   :ns-whitelist [;; "carneades.*"
                  ]
   :shared-appender-config {:rotor
                            {:path "carneades.log" :max-size 10000 "backlog" 10}}
   :timestamp-pattern "MMM-dd HH:mm"
   :fmt-output-fn
   (fn [{:keys [level throwable message timestamp hostname ns]}
        ;; Any extra appender-specific opts:
        & [{:keys [nofonts?] :as appender-fmt-output-opts}]]
     (format "%s %s [%s] - %s"
             timestamp (-> level name str/upper-case) ns (or message "")))})

(timbre/merge-config! logger-config)

(defn init
  "Called when app is deployed as a servlet on an app server such as
   Tomcat. Put any initialization code here."
  []
  (timbre/merge-config! logger-config)
  (service/start)
  (info "Carneades started successfully."))

(defn destroy
  "destroy will be called when your application
   shuts down, put any clean up code here"
  []
  (service/stop)
  (println "shutting down..."))

(defn wrap-file
  "Like ring.middleware.file/wrap-file but does not ensure that the
  directory exists. It prevents deployment error if the directory does
  not exists and the handler is compiled but no used."
  [app ^String root-path & [opts]]
  (let [opts (merge {:root root-path, :index-files? true, :allow-symlinks? false} opts)]
    (fn [req]
      (if-not (= :get (:request-method req))
        (app req)
        (let [path (.substring ^String (codec/url-decode (:uri req)) 1)]
          (or (response/file-response path opts)
              (app req)))))))

(def all-routes [carneades-web-routes app-routes])

(def tomcat-all-routes [tomcat-carneades-web-routes tomcat-app-routes])

(def tomcat-app (-> (apply routes tomcat-all-routes)
                    (session/wrap-stateful-session)
                    (wrap-keyword-params)
                    (wrap-params)
                    (wrap-multipart-params)))

(def tomcat-war-handler (middleware/war-handler tomcat-app))

(def app (-> (apply routes all-routes)
             (session/wrap-stateful-session)
             (wrap-keyword-params)
             (wrap-params)
             (wrap-multipart-params)
             (wrap-file "../client/dist")))

(def war-handler (middleware/war-handler app))
