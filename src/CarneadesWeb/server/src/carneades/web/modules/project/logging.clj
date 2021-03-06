;; Copyright (c) 2014 Fraunhofer Gesellschaft
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns carneades.web.modules.project.logging
  (:require ;[carneades.web.modules.project.functions :refer [get-projects]]
            [dire.core :refer [with-pre-hook!]]
            [taoensso.timbre :as timbre :refer (trace debug info warn error fatal spy)]))

;; (with-pre-hook! #'get-resource  (fn
;;                                   ([] (debug "Retrieve all projects."))
;;                                   ([id] (debug "Retrieve projects:" id))))


