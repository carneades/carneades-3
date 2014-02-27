;; Copyright (c) 2014 Fraunhofer Gesellschaft
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns carneades.web.modules.arguments.functions
  (:require [clj-http.client :as client]
            [compojure.route :as route]
            [clojure.data.json :as json]))

;; "http://localhost:3000/carneadesws/argumentgraph-info"

(defonce ws-path "http://localhost:3000/carneadesws")
              
(defn get-argumentsgraph-info
  [project db id] 
  (json/read-str (:body (client/get (str ws-path "/metadata/" project "/" db "/" id))) :key-fn keyword))

(defn get-main-issues
  [project db]
  (json/read-str (:body (client/get (str ws-path "/main-issues/" project "/" db))) :key-fn keyword))

(defn get-outline
  [project db]
  (json/read-str (:body (client/get (str ws-path "/outline/" project "/" db))) :key-fn keyword))

(defn get-references
  [project db]
  (json/read-str (:body (client/get (str ws-path "/metadata/" project "/" db))) :key-fn keyword))
