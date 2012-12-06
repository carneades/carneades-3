;;; Copyright (c) 2012 Fraunhofer Gesellschaft
;;; Licensed under the EUPL V.1.1

(ns impact.web.routes-selfexe
  (:use impact.web.routes-dev
        compojure.core
        ring.adapter.jetty
        [hiccup.middleware :only (wrap-base-url)]
        [clojure.java.browse :only [browse-url]])
  (:require [compojure.handler :as handler])
  (:gen-class))

(defn start-server
  []
  (run-jetty #'impact-app {:join? false :port 8080}))

(defn -main
  [& args]
  (start-server)
  (browse-url "http://localhost:8080/policymodellingtool/home.html"))