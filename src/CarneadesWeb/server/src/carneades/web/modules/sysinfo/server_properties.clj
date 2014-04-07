;; Copyright (c) 2012 Fraunhofer Gesellschaft
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns carneades.web.modules.polican.logic.server-properties)

(defn- format-values
  [vals]
  (apply str (map #(str % "\n") vals)))

(defn server-properties
  []
  (str
   "========== JAVA PROPERTIES ==========\n\n\n"
   (format-values (System/getProperties))
   "\n\n\n========== ENVIRONMENT ==========\n\n\n"
   (format-values (System/getenv))))

