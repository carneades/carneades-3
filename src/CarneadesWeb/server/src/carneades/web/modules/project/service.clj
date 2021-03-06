;; Copyright (c) 2014 Fraunhofer Gesellschaft
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns ^{:doc "Functions from the old webservice"}
  carneades.web.modules.project.service
  (:require 
   [carneades.database.db :as db]
   [carneades.web.modules.project.pack :as p]
   [carneades.database.argument-graph :as ag-db]
   [carneades.web.modules.project.outline :refer [create-outline]]
   [carneades.admin.project :as project]
   [carneades.engine.utils :as f]
   [clojure.java.io :as io]
   [carneades.web.system :as s]
   [carneades.config.config :refer [get-projects-directory]]
   [taoensso.timbre :as timbre :refer [trace debug info warn error fatal spy]]
   [carneades.database.evaluation :as eval]))

(def default-user "root")
(def default-password "pw1")

(defn- get-project-properties
  [id]
  (merge (get-in (deref s/state) [:projects-data id :properties])
         {:id id}))

(defn- get-project-theories
  [id]
  (get-in (deref s/state) [:projects-data id :available-theories]))

(defmacro with-evaluation
  [instruction]
  `(let [r# ~instruction]
     (eval/evaluate-graph')
     r#))

(defn get-projects
  []
  (let [s (deref s/state)]
    (reduce
     (fn [projects id]
       (let [props (get-in s [:projects-data id :properties])]
         (conj projects (merge props {:id id}))))
     []
     (:projects s))))

(defn get-project
  [id]
  (get-project-properties id))

(defn post-project
  [name properties]
  (project/create-project name default-user default-password properties)
  (s/init-data)
  name)

(defn post-ag
  [pid name metadata]
  (ag-db/create-argument-database pid name default-user default-password metadata)
  name)

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

(defn put-statement
  [project db id update]
  (let [dbconn (db/make-connection project db default-user default-password)]
    (db/with-db dbconn
      (with-evaluation
        (ag-db/update-statement id update)))))

(defn post-statement
  [project db statement]
  (let [dbconn (db/make-connection project db default-user default-password)]
    (db/with-db dbconn
      (with-evaluation
        (ag-db/create-statement (p/unpack-statement statement))))))

(defn delete-statement
  [project db id]
  (let [dbconn (db/make-connection project db default-user default-password)]
    (db/with-db dbconn
      (with-evaluation
        (ag-db/delete-statement id)))))

(defn get-metadatum
  [project db id]
  (let [dbconn (db/make-connection project db "guest" "")]
    (db/with-db dbconn
      (ag-db/read-metadata id))))

(defn put-metadatum
  [project db id update]
  (let [dbconn (db/make-connection project db default-user default-password)]
    (db/with-db dbconn
      (ag-db/update-metadata id update))))

(defn get-metadata
  [project db]
  (let [dbconn (db/make-connection project db "guest" "")]
    (db/with-db dbconn
      (ag-db/list-metadata))))

(defn get-argument
  [project db id]
  (let [dbconn (db/make-connection project db "guest" "")]
    (db/with-db dbconn
      (p/pack-argument (ag-db/read-argument (str id))))))

(defn get-arguments
  [project db]
  (let [dbconn (db/make-connection project db "guest" "")]
    (db/with-db dbconn
      (map p/pack-argument (ag-db/list-arguments)))))

(defn put-argument
  [project db id update]
  (let [dbconn (db/make-connection project db default-user default-password)]
    (db/with-db dbconn
      (with-evaluation
        (ag-db/update-argument id update)))))

(defn post-argument
  [project db arg]
  (let [dbconn (db/make-connection project db default-user default-password)]
    (db/with-db dbconn
      (with-evaluation
        (ag-db/create-argument (p/unpack-argument arg))))))

(defn delete-argument
  [project db id]
  (let [dbconn (db/make-connection project db default-user default-password)]
    (db/with-db dbconn
      (with-evaluation
        (ag-db/delete-argument id)))))

(defn get-outline
  [project db]
  (let [dbconn (db/make-connection project db "guest" "")]
    (db/with-db dbconn
      {:outline (create-outline (map p/pack-statement (ag-db/main-issues)) 5)})))

(defn get-theme
  [project doc]
  (let [path (str (get-projects-directory) f/file-separator project f/file-separator
                  "theme" f/file-separator doc)]
    (when (f/exists? path)
      (io/input-stream path))))

(defn get-theories
  [tid]
  {:theories (get-project-theories tid)})


