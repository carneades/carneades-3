;; Copyright (c) 2014 Fraunhofer Gesellschaft
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns carneades.web.system
  (:require [carneades.web.project :refer [init-projects-data!]]
            [carneades.project.admin :as p]))

(def state (atom nil))

(defn- init-projects-data
  []
  {:projects (p/list-projects)
   :projects-data (init-projects-data!)})

(defn init
  []
  (reset! state (init-projects-data)))
