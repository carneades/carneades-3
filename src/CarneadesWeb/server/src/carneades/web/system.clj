;; Copyright (c) 2014 Fraunhofer Gesellschaft
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns carneades.web.system
  ^{:author "Sebastian Kaiser"
    :doc "Handler for the system"}
  (:use ring.server.standalone [ring.middleware file-info file])
  (:require [taoensso.timbre :as timbre :refer (trace debug info warn error fatal spy)]
            [com.postspectacular.rotor :as rotor]
            [carneades.web.handler :as handler]
            [ring.middleware.format-params :as format-params]
            [ring.middleware.params :refer [wrap-params]]
            [cheshire.core :as json]))

(defn init
  "init will be called once when
   app is deployed as a servlet on
   an app server such as Tomcat
   put any initialization code here"
  []
  (timbre/merge-config!
   {:appenders {:rotor {:min-level :info
                        :enabled? true
                        :async? false          ; should be always false for rotor
                        :max-message-per-msecs nil
                        :fn rotor/append}}})

  (timbre/merge-config!
   {:appenders {:standard-out {:min-level :debug
                               :enabled? true
                               :async? false
                               :max-message-per-msecs nil}}})

  (timbre/set-config!
    [:shared-appender-config :rotor]
    {:path "carneades.web.log" :max-size 10000 "backlog" 10})

  (info "logging successfully initiated."))

(defn get-handler []
  ;; #'app expands to (var app) so that when we reload our code,
  ;; the server is forced to re-resolve the symbol in the var
  ;; rather than having its own copy. When the root binding
  ;; changes, the server picks it up without having to restart.
  (-> #'handler/app
    ;; Makes static assets in $PROJECT_DIR/resources/public/ available.
    (wrap-file "../client/dist")
    ;; Content-Type, Content-Length, and Last Modified headers for files in body
    (wrap-file-info)
    ;; Support for JSON POST BODY as parameters
    (format-params/wrap-format-params
     :predicate format-params/json-request?
     :decoder #(json/parse-string % true)
     :charset format-params/get-or-guess-charset)
    ))

(defn start-server
  "used for starting the server in development mode from REPL"
  [system & [port]]
  (let [port (if port
               port
               8080)
        server (:server system)]
    (reset! server
            (serve (get-handler)
                   {:port port
                    :init init
                    :auto-reload? true
                    ;:destroy destroy
                    :join true}))
    (info (str "You can view the site at http://localhost:" port))))

(defn stop-server [system]
  (let [server (:server system)]
    (.stop @server)
    (reset! server nil)))


(def system {:server (atom nil)})

(defn start
  [system]
  (start-server system 3000)
  (info "system started :-)")
  system)

(defn stop
  [system]
  (stop-server system)
  (info "system stopped :-(")
  {:server (atom nil)})
