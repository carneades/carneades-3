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
