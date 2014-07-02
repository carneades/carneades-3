;; Copyright (c) 2014 Fraunhofer Gesellschaft
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns ^{:doc "Functions from the old webservice"}
  carneades.web.modules.project.service
  (:require 
   [carneades.database.db :as db]
   [carneades.web.pack :as p]
   [carneades.database.argument-graph :as ag-db]
   [carneades.web.outline :refer [create-outline]]
   [carneades.project.admin :as project]
   [carneades.engine.utils :as f]
   [clojure.java.io :as io]
   [carneades.web.handler :as handler]
   [carneades.web.project :as pr]))

(defn get-projects
  []
  (let [s (deref handler/state)]
    (reduce
     (fn [projects id]
       (let [props (get-in s [:projects-data id :properties])]
         (conj projects (merge props {:id id}))))
     []
     (:projects s))))

(defn get-project
  [id]
  (pr/get-project-properties id handler/state))

(defn get-statement
  [project db id]
  (let [dbconn (db/make-connection project db "guest" "")]
    (db/with-db dbconn
      (p/pack-statement (ag-db/read-statement id)))))

(defn get-statements
  [project db]
  (let [dbconn (db/make-connection project db "guest" "")]
    (db/with-db dbconn
      (map p/pack-statement (ag-db/list-statements)))))

(defn get-metadatum
  [project db id]
  (let [dbconn (db/make-connection project db "guest" "")]
    (db/with-db dbconn
      (ag-db/read-metadata id))))

(defn get-metadata
  [project db]
  (let [dbconn (db/make-connection project db "guest" "")]
    (db/with-db dbconn
      (ag-db/list-metadata))))

(defn get-arguments
  [project db]
  (let [dbconn (db/make-connection project db "guest" "")]
    (db/with-db dbconn
      (map p/pack-argument (ag-db/list-arguments)))))

(defn get-argument
  [project db id]
  (let [dbconn (db/make-connection project db "guest" "")]
    (db/with-db dbconn
      (p/pack-argument (ag-db/read-argument id)))))

(defn get-outline
  [project db]
  (let [dbconn (db/make-connection project db "guest" "")]
    (db/with-db dbconn
      {:outline (create-outline (map p/pack-statement (ag-db/main-issues)) 5)})))

(defn get-theme
  [project doc]
  (let [path (str project/projects-directory f/file-separator project f/file-separator
                  "theme" f/file-separator doc)]
    (if (not (f/exists? path))
      {:status 404
       :body "File not found"}
      (io/input-stream path))))

(defn get-theories
  [tid]
  {:theories (pr/get-project-theories tid handler/state)})


