;; Copyright (c) 2014 Fraunhofer Gesellschaft
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns carneades.web.modules.lican.entity
  (:require [carneades.web.modules.lican.triplestore :as triplestore]
            [carneades.project.fs :as project]))

(defn get-software-entity
  "Returns information about the software entity"
  [project uri]
  (let [properties (project/load-project-properties project)
        triplestore (:triplestore properties)
        repo-name (:repo-name properties)
        markos-namespaces (:namespaces properties)]
    {:name (triplestore/get-entity-name triplestore repo-name markos-namespaces uri)
     :uri uri}))
