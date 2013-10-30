(ns carneades.engine.system
  (:require [clj-logging-config.log4j :refer :all]))

(defn set-loggers
  []
  (set-logger! :pattern "[%c]%n%m%n" :level :debug)
  (set-loggers! "org.xml.sax.XMLReader" {:level :info}))

(defn start
  [])

(defn stop
  [])

(set-loggers)
