;; Copyright (c) 2013 Fraunhofer Gesellschaft
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

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
