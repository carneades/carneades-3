;; Copyright (c) 2014 Fraunhofer Gesellschaft
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns carneades.web.modules.project.routes
  ^{:doc "Definition of project routes."}
  (:require [carneades.web.modules.session.logic :refer [session-put-language]]
            [carneades.web.modules.project.logic :refer :all]
            [sandbar.stateful-session :refer :all]
            [clojure.string :refer [split]]
            [ring.middleware.session.cookie :refer :all]
            [ring.middleware.json :refer [wrap-json-response]]
            [compojure.core :refer [defroutes context ANY GET]]
            [liberator.core :refer [defresource request-method-in]]
            [cheshire.core :refer :all]
            [clojure.set :as set]
            [clabango.parser :as parser]
            [noir.request :refer :all]
            [taoensso.timbre :as timbre :refer [debug info spy]])
  (:import java.net.URL))

(defresource list-metadata-resource [pid db k]
  :available-media-types ["application/json"]
  :allowed-methods [:get]
  :available-charsets["utf-8"]
  :exists? (fn [_] (session-put-language nil) {:language (session-get :language)})
  :handle-ok (fn [{{{host "host"} :headers} :request lang :language}]
               (get-metadata [pid db] :k k :host host :lang (keyword lang))))

(defresource list-reference-resource [pid db]
  :available-media-types ["application/json"]
  :allowed-methods [:get]
  :available-charsets["utf-8"]
  :exists? (fn [_] (session-put-language nil) {:language (session-get :language)})
  :handle-ok (fn [{{{host "host"} :headers} :request lang :language}]
               (get-references [pid db] :host host :lang (keyword lang))))

(defresource list-outline-resource [pid db]
  :available-media-types ["application/json"]
  :allowed-methods [:get]
  :available-charsets["utf-8"]
  :exists? (fn [_] (session-put-language nil) {:language (session-get :language)})
  :handle-ok (fn [{{{host "host"} :headers} :request lang :language}]
               (get-outline [pid db] :host host :lang (keyword lang))))

(defresource entry-outline-resource [pid db id]
  :available-media-types ["application/json"]
  :allowed-methods [:get]
  :available-charsets["utf-8"]
  :exists? (fn [_] (session-put-language nil) {:language (session-get :language)})
  :handle-ok (fn [{{{host "host"} :headers} :request lang :language}]
               (get-outline pid db id :host host :lang (keyword lang))))

(defresource list-issue-resource [pid db]
  :available-media-types ["application/json"]
  :allowed-methods [:get]
  :available-charsets["utf-8"]
  :exists? (fn [_] (session-put-language nil) {:language (session-get :language)})
  :handle-ok (fn [{{{host "host"} :headers} :request lang :language}]
               (get-issues [pid db] :host host :lang (keyword lang))))

(defresource entry-metadata-resource [pid db id]
  :available-media-types ["application/json"]
  :allowed-methods [:get]
  :available-charsets["utf-8"]
  :exists? (fn [_] (session-put-language nil) {:language (session-get :language)})
  :handle-ok (fn [{{{host "host"} :headers} :request lang :language}]
               (get-metadatum [pid db id] :host host :lang (keyword lang))))

(defresource list-argument-resource [pid db]
  :available-media-types ["application/json"]
  :allowed-methods [:get]
  :available-charsets["utf-8"]
  :exists? (fn [_] (session-put-language nil) {:language (session-get :language)})
  :handle-ok (fn [{{{host "host"} :headers} :request lang :language}]
               (get-argument [pid db] :host host :lang (keyword lang))))

(defresource edit-argument-resource [pid db]
  :available-media-types ["application/json"]
  :allowed-methods [:get]
  :available-charsets["utf-8"]
  :exists? (fn [_] (session-put-language nil) {:language (session-get :language)})
  :handle-ok (fn [{{{host "host"} :headers} :request lang :language}]
               (get-argument [pid db] :host host :lang (keyword lang))))

(defresource entry-argument-resource [pid db id]
  :available-media-types ["application/json"]
  :allowed-methods [:get]
  :available-charsets["utf-8"]
  :exists? (fn [_] (session-put-language nil) {:language (session-get :language)})
  :handle-ok (fn [{{{host "host"} :headers} :request lang :language}]
               (get-argument [pid db id] :host host :lang (keyword lang))))

(defresource list-node-resource [pid db]
  :available-media-types ["application/json"]
  :allowed-methods [:get]
  :available-charsets["utf-8"]
  :exists? (fn [_] (session-put-language nil) {:language (session-get :language)})
  :handle-ok (fn [{{{host "host"} :headers} :request lang :language}]
               (get-nodes pid db 1 :host host :lang (keyword lang))))

(defresource entry-node-resource [pid db id]
  :available-media-types ["application/json"]
  :allowed-methods [:get]
  :available-charsets["utf-8"]
  :exists? (fn [_] (session-put-language nil) {:language (session-get :language)})
  :handle-ok (fn [{{{host "host"} :headers} :request lang :language}]
               (get-nodes pid db id :host host :lang (keyword lang))))

(defresource list-statement-resource [pid db]
  :available-media-types ["application/json"]
  :allowed-methods [:get]
  :available-charsets["utf-8"]
  :exists? (fn [_]
             (session-put-language nil)
             {:language (session-get :language)})
  :handle-ok (fn [{{{host "host"} :headers} :request lang :language}]
               (let [s (get-statement [pid db] :host host :lang (keyword lang))]
                 (debug "s = " s)
                 s)))

(defresource entry-statement-resource [pid db id]
  :available-media-types ["application/json"]
  :allowed-methods [:get]
  :available-charsets["utf-8"]
  :exists? (fn [_] (session-put-language nil) {:language (session-get :language)})
  :handle-ok (fn [{{{host "host"} :headers} :request lang :language}]
               (get-statement [pid db id] :host host :lang (keyword lang))))

(defresource entry-project-resource [id]
  :available-media-types ["application/json"]
  :available-charsets ["utf-8"]
  :allowed-methods [:get]
  :exists? (fn [_] (session-put-language nil) {:language (session-get :language)})
  :handle-ok (fn [{{{host "host"} :headers} :request lang :language}]
               (get-project :id id :lang (keyword lang) :host host)))

;; (defresource entry-download-project-resource [id]
;;   :available-media-types ["application/zip"]
;;   :available-charsets ["utf-8"]
;;   :allowed-methods [:get]
;;   :handle-ok (fn [{{{host "host"} :headers} :request}]
;;                (get-project-archive :project id :host host)))

;; (defresource entry-upload-project-resource [file]
;;   :available-media-types ["application/octet-stream"]
;;   :available-charsets ["utf-8"]
;;   :allowed-methods [:post]
;;   :post! (fn [{{{host "host"} :headers} :request}]
;;            (post-project-archive :file file :host host)))

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
  :handle-ok (fn [{{{host "host"} :headers} :request lang :language}]
               (generate-string (get-theories {:tpid pid :host host :lang lang}))))

(defresource entry-theories-resource [params]
  :available-media-types ["application/json"]
  :available-charsets ["utf8"]
  :allowed-methods [:get]
  :exists? (fn [_] (session-put-language nil) {:language (session-get :language)})
  :handle-ok (fn [{lang :language}]
               (info "params:" params)
               (generate-string (assoc (get-theories params)
                                  :lang lang))))

(defresource entry-theme-css-resource [pid did]
  :available-media-types ["text/css"]
  :available-charsets ["utf8"]
  :allowed-methods [:get]
  :handle-ok (fn [{{{host "host"} :headers} :request}] (get-theme [pid did] :host host)))

(defresource entry-theme-html-resource [pid did]
  :available-media-types ["text/html"]
  :available-charsets ["utf8"]
  :allowed-methods [:get]
  :handle-ok (fn [{{{host "host"} :headers} :request}] (get-theme [pid did] :host host)))

(defresource entry-theme-png-resource [pid did]
  :available-media-types ["image/png"]
  :available-charsets ["utf8"]
  :allowed-methods [:get]
  :handle-ok (fn [{{{host "host"} :headers} :request}] (get-theme [pid did] :host host)))

(defresource entry-map-resource [pid db]
  :available-media-types ["image/svg+xml"]
  :available-charsets ["utf-8"]
  :allowed-methods [:get]
  :exists? (fn [_] (session-put-language nil) {:language (session-get :language)})
  :handle-ok (fn [{{{host "host"} :headers} :request lang :language}]
               (get-argument-map pid db :lang (keyword lang) :host host)))

(defresource legal-profiles-resources [pid profile]
  :available-media-types ["application/json"]
  :allowed-methods [:get :post]
  :available-charsets["utf-8"]
  :handle-created (fn [ctx]
                    {:id (::id ctx)})
  :handle-ok (fn [ctx]
               (get-profiles pid))
  :post! (fn [_]
           {::id (post-profile pid profile)}))

(defresource entry-legal-profiles-resource [pid id update]
  :available-media-types ["application/json"]
  :allowed-methods [:get :put :delete]
  :available-charsets ["utf-8"]
  :exists? (fn [ctx]
             (when-let [p (get-profile pid id)]
               {::entry p}))
  :put! (fn [ctx]
          (put-profile pid id update))
  :delete! (fn [ctx]
             (delete-profile pid id))
  :handle-ok ::entry)

(defroutes carneades-projects-api-routes
  (ANY "/" [] (list-project-resource))
  ;; (ANY "/upload" [file] (entry-upload-project-resource file))

  (context "/:pid" [pid]
           (ANY "/" [] (entry-project-resource pid))
           ;; (ANY "/download" [] (entry-download-project-resource pid))

           (context "/theme" []
                    (ANY "/css/:did" [did] (entry-theme-css-resource pid did))
                    (ANY "/html/:did" [did] (entry-theme-html-resource pid did))
                    (ANY "/png/:did" [did] (entry-theme-png-resource pid did)))

           (context "/theories" []
                    (ANY "/" [] (list-theories-resource pid))
                    (ANY "/:tpid/:tid" {params :params} (entry-theories-resource params)))

           (context "/legalprofiles" []
             (ANY "/" req (legal-profiles-resources pid (:body req)))
             (ANY "/:id" req (entry-legal-profiles-resource pid
                                                            (-> req :params :id)
                                                            (:body req))))

           (context "/:db" [db]
                    (context "/metadata" []
                             (ANY "/references" [] (list-reference-resource pid db))
                             (ANY "/" [k] (list-metadata-resource pid db k))
                             (ANY "/:mid" [mid] (entry-metadata-resource pid db mid)))

                    (context "/outline" []
                             (ANY "/" [] (list-outline-resource pid db))
                             (ANY "/issues" [] (list-issue-resource pid db)))

                    (context "/arguments" []
                             (ANY "/" [] (list-argument-resource pid db))
                             (ANY "/:aid" [aid] (entry-argument-resource pid db aid))
                             (ANY "/edit/:aid" [] (edit-argument-resource)))

                    (context "/statements" []
                             (ANY "/" [] (list-statement-resource pid db))
                             (ANY "/:sid" [sid] (entry-statement-resource pid db sid)))

                    (context "/nodes" []
                      (ANY "/" [] (list-node-resource pid db))
                             (ANY "/:nid" [nid] (entry-node-resource pid db nid)))

                    (context "/map" []
                      (ANY "/" [] (entry-map-resource pid db))))))
