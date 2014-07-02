(ns carneades.web.repl
  ^{:author "Sebastian Kaiser"}
  (:require [taoensso.timbre :as timbre :refer (trace debug info warn error fatal spy)]
            [carneades.web.system :as system]
            [carneades.web.handler :refer [app init destroy]]
            [ring.server.standalone :refer [serve]]
            [ring.middleware.file :refer [wrap-file]]
            [ring.middleware.file-info :refer [wrap-file-info]]
            [ring.middleware.format-params :as format-params]
            [cheshire.core :as json]))

(def system {:server (atom nil)})

(defn get-handler []
  ;; #'app expands to (var app) so that when we reload our code,
  ;; the server is forced to re-resolve the symbol in the var
  ;; rather than having its own copy. When the root binding
  ;; changes, the server picks it up without having to restart.
  (-> #'app
      (wrap-file "../client/dist")
      (wrap-file-info)))

(defn start-server
  "used for starting the server in development mode from REPL"
  [system & [port]]
  (let [port (if port (Integer/parseInt port) 8080)
        server (:server system)]
    (reset! server
            (serve (get-handler)
                   {:port port
                    :init init
                    :auto-reload? true
                    :destroy destroy
                    :join? false}))
    (system/start)
    (info (str "You can view the site at http://localhost:" port))))

(defn stop-server [system]
  (let [server (:server system)]
    (.stop @server)
    (system/stop)
    (reset! server nil)))
