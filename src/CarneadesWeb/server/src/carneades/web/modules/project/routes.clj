;; Copyright (c) 2014 Fraunhofer Gesellschaft
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns carneades.web.modules.project.routes
  ^{:author "Sebastian Kaiser"
    :doc "Definition of project routes."}
  (:require [carneades.web.modules.session.logic :refer [session-put-language]]
            [carneades.web.modules.project.functions
             :refer [get-projects
                     get-metadata
                     get-outline
                     get-statements
                     get-arguments
                     get-arguments-dbg
                     get-nodes
                     get-argument-map
                     get-theories]]
            [sandbar.stateful-session :refer :all]
            [ring.middleware.session.cookie :refer :all]
            [ring.middleware.json :refer [wrap-json-response]]
            [compojure.core :refer [defroutes context ANY GET]]
            [liberator.core :refer [defresource]]
            ;; [carneades.web.modules.project.handlers :as handlers]
            ;; [carneades.web.modules.project.logging :as logging
            [cheshire.core :refer :all]
            [clojure.set :as set]
            [clabango.parser :as parser]
            [noir.request :refer :all]))

(defresource list-metadata-resource [pid db k]
  :available-media-types ["application/json"]
  :allowed-methods [:get]
  :available-charsets["utf-8"]
  :handle-ok (fn [{{{host "host"} :headers} :request}] (get-metadata [pid db] :k k :host host)))

(defresource entry-metadata-resource [pid db id]
  :available-media-types ["application/json"]
  :allowed-methods [:get]
  :available-charsets["utf-8"]
  :handle-ok (fn [{{{host "host"} :headers} :request}] (get-metadata [pid db id] :host host)))

(defresource list-argument-resource [pid db]
  :available-media-types ["application/json"]
  :allowed-methods [:get]
  :available-charsets["utf-8"]
  :handle-ok (fn [{{{host "host"} :headers} :request}] (get-arguments [pid db] :host host)))

(defresource edit-argument-resource [pid db]
  :available-media-types ["application/json"]
  :allowed-methods [:get]
  :available-charsets["utf-8"]
  :handle-ok (fn [{{{host "host"} :headers} :request}] (get-arguments [pid db] :host host)))

(defresource entry-argument-resource [pid db id]
  :available-media-types ["application/json"]
  :allowed-methods [:get]
  :available-charsets["utf-8"]
  :handle-ok (fn [{{{host "host"} :headers} :request}] (get-arguments [pid db id] :host host)))

(defresource entry-argument-dbg-resource [pid db id]
  :available-media-types ["application/json"]
  :allowed-methods [:get]
  :available-charsets["utf-8"]
  :handle-ok (fn [{{{host "host"} :headers} :request}] (get-arguments-dbg [pid db id] :host host)))

(defresource list-node-resource [pid db]
  :available-media-types ["application/json"]
  :allowed-methods [:get]
  :available-charsets["utf-8"]
  :handle-ok (fn [{{{host "host"} :headers} :request}] (get-nodes pid db 1 :host host)))

(defresource entry-node-resource [pid db id]
  :available-media-types ["application/json"]
  :allowed-methods [:get]
  :available-charsets["utf-8"]
  :handle-ok (fn [{{{host "host"} :headers} :request}] (get-nodes pid db id :host host)))

(defresource list-statement-resource [pid db]
  :available-media-types ["application/json"]
  :allowed-methods [:get]
  :available-charsets["utf-8"]
  :handle-ok (fn [{{{host "host"} :headers} :request}] (get-statements [pid db] :host host)))

(defresource entry-statement-resource [pid db id]
  :available-media-types ["application/json"]
  :allowed-methods [:get]
  :available-charsets["utf-8"]
  :handle-ok (fn [{{{host "host"} :headers} :request}] (get-statements [pid db id] :host host)))

(defresource entry-project-resource [id]
  :available-media-types ["application/json"]
  :available-charsets ["utf-8"]
  :allowed-methods [:get]
  :exists? (fn [_] (session-put-language nil) {:language (session-get :language)})
  :handle-ok (fn [{{{host "host"} :headers} :request lang :language}]
               (get-projects :id id :lang (keyword lang) :host host)))

(defresource list-project-resource []
  :allowed-methods [:get :post]
  :available-charsets ["utf-8"]
  :available-media-types ["application/json"]
  :exists? (fn [_] (session-put-language nil) {:language (session-get :language)})
  :handle-ok (fn [{{{host "host"} :headers} :request lang :language}]
               (get-projects :lang (keyword lang) :host host)))

(defresource list-theories-resource [pid]
  :allowed-methods [:get]
  :available-charsets ["utf-8"]
  :available-media-types ["application/json"]
  :handle-ok (fn [_] (generate-string (get-theories :pid pid))))

(defresource entry-theories-resource [params]
  :available-media-types ["application/json"]
  :available-charsets ["utf8"]
  :allowed-methods [:get]
  :exists? (fn [_] (session-put-language nil) {:language (session-get :language)})
  :handle-ok (fn [{lang :language}]
               (generate-string (assoc (get-theories params)
                                  :lang lang))))

(defresource entry-map-resource [pid db]
  :available-media-types ["image/svg+xml"]
  :available-charsets ["utf-8"]
  :allowed-methods [:get]
  :exists? (fn [_] (session-put-language nil) {:language (session-get :language)})
  :handle-ok (fn [{{{host "host"} :headers} :request lang :language}]
               (get-argument-map pid db :lang (keyword lang) :host host)))

(defroutes carneades-projects-api-routes

  (ANY "/" [] (list-project-resource))
  (context "/:pid" [pid]

    (ANY "/" [] (entry-project-resource pid))

    (context "/theories" []
       (ANY "/" [] (list-theories-resource pid))
       (ANY "/:tid" {params :params} (entry-theories-resource params)))

    (context "/:db" [db]

      (context "/metadata" []
        (ANY "/" [k] (list-metadata-resource pid db k))
        (ANY "/:mid" [mid] (entry-metadata-resource pid db mid)))

      (context "/arguments" []
        (ANY "/" [] (list-argument-resource pid db))
        (ANY "/:aid" [aid] (entry-argument-resource pid db aid))
        (ANY "/dbg/:aid" [aid] (entry-argument-dbg-resource pid db aid))
        (ANY "/edit/:aid" [] (edit-argument-resource)))

      (context "/statements" []
        (ANY "/" [] (list-statement-resource pid db))
        (ANY "/:sid" [sid] (entry-statement-resource pid db sid)))

      (context "/nodes" []
        (ANY "/" [] (list-node-resource pid db))
        (ANY "/:nid" [nid] (entry-node-resource pid db nid)))

      (context "/map" []
        (ANY "/" [] (entry-map-resource pid db))))))
