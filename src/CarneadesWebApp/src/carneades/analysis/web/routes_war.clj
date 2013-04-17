;;; Copyright (c) 2012 Fraunhofer Gesellschaft
;;; Licensed under the EUPL V.1.1

(ns carneades.analysis.web.routes-war
  (:use carneades.policy-analysis.web.routes
        compojure.core
        [hiccup.middleware :only (wrap-base-url)])
  (:require [compojure.handler :as handler]))

(def carneades-webpp
  (-> (handler/site policy-analysis-routes)
      (wrap-base-url)))
