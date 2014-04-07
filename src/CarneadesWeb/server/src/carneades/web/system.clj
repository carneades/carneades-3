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
            [ring.middleware.format-params :as format-params]
            [ring.middleware.params :refer [wrap-params]]
            [ring.middleware.json :refer [wrap-json-response]]
            [cheshire.core :as json]
            [carneades.web.service :as service]
            [compojure.core :refer :all]
            [noir.util.middleware :as middleware]
            [sandbar.stateful-session :refer :all]
            [carneades.web.routes :refer [carneades-web-routes]]
            [compojure.route :as route :refer [files resources not-found]]
            [sandbar.stateful-session :as session]))
