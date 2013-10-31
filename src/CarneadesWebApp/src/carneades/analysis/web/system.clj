(ns carneades.analysis.web.system
  (:require [clj-logging-config.log4j :refer :all]
            [carneades.analysis.web.routes-dev :as dev]
            [ring.adapter.jetty :as jetty]
            [carneades.web.service :as service]
            [carneades.engine.system :as engine]
            [carneades.analysis.web.routes-war]))

(defn system
  [& {mode :mode}]
  {:server nil
   :mode (or mode :dev)})

(defn set-loggers
  []
  (set-logger! :pattern "[%c]%n%m%n" :level :debug)
  (set-loggers! "org.mortbay.log" {:level :info}
                "httpclient.wire.header" {:level :info}
                "httpclient.wire.content" {:level :info}
                "org.apache.commons.httpclient.HttpMethodBase" {:level :info}
                "org.apache.commons.httpclient.methods.EntityEnclosingMethod" {:level :info}
                "org.openrdf.http.client.HTTPClient" {:level :info}
                "org.apache.commons.httpclient.HttpConnection" {:level :info}
                "org.apache.commons.httpclient.MultiThreadedHttpConnectionManager" {:level :info}
                "org.apache.commons.httpclient.util.IdleConnectionHandler" {:level :info}
                "org.xml.sax.XMLReader" {:level :info}))

(defn start
  [system]
  (set-loggers)
  (engine/start)
  (service/start)
  (let [server (condp = (:mode system)
                 :standalone
                 (jetty/run-jetty #'dev/carneades-webapp
                                  {:join? false :port 8080})

                 :dev
                 (jetty/run-jetty #'dev/carneades-webapp
                                  {:join? false :port 8080})

                 :war
                 (jetty/run-jetty #'carneades.analysis.web.routes-war/carneades-webapp
                                  {:join? false :port 8080}))]
   (assoc system :server server)))

(defn stop
  [system]
  (service/stop)
  (engine/stop)
  (.stop (:server system)))

(set-loggers)
