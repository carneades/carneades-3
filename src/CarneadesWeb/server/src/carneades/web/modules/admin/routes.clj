;; Copyright (c) 2014 Fraunhofer Gesellschaft
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns carneades.web.modules.admin.routes
  (:use compojure.core)
  ;; (:require [carneades.web.modules.admin.views.layout :as layout])
  )

(defn admin-project-page []
  ;; (layout/render "home.html" {:projects "abc"
  ;;                             :base "/carneades"
  ;;                             :context2 "/carneades/projects"})
  )

(defroutes admin-routes*
  (GET "/projects" [] (admin-project-page)))

(def admin-routes (-> #'admin-routes*))

(defroutes admin-web-routes
  (ANY "*" [] (context "/admin" [] admin-routes)))
