(ns carneades.analysis.web.system
  (:require [clj-logging-config.log4j :refer :all]
            [carneades.analysis.web.routes-dev :as dev]
            [ring.adapter.jetty :as jetty]
            [carneades.web.service :as service]))

(defn system
  []
  {:server nil})

(defn start
  [system]
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
                "org.xml.sax.XMLReader" {:level :info})
  (service/start)
  (assoc system :server (jetty/run-jetty #'dev/carneades-webapp {:join? false :port 8080})))

(defn stop
  [system]
  (service/stop)
  (.stop (:server system)))
