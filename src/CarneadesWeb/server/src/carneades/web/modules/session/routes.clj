;; Copyright (c) 2014 Fraunhofer Gesellschaft
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns carneades.web.modules.session.routes
  ^{:author "Sebastin Kaiser"
    :doc "Definition of session routes"}
  (:require [carneades.web.modules.session.logic
             :refer [session-put-language]]
            [compojure.core :refer [defroutes context ANY GET]]
            [liberator.core :refer [defresource]]
            [sandbar.stateful-session :refer :all]
            [ring.middleware.session.cookie :refer :all]))

(defresource entry-language-resource [lang]
  :available-media-types ["application/json"]
  :allowed-methods [:get]
  :available-charsets ["utf-8"]
  :exists? (fn [_]
             (if (some #{lang} '("en" "de" "nl" "fr" "it" "sp"))
               (session-put-language lang)
               (session-put-language "en"))
             {:language (session-get :language)})
  :handle-ok (fn [{lang :language}] {:language lang}))

(defroutes carneades-session-api-routes
  (ANY "/" [l] (entry-language-resource l)))
