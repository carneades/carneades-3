;; Copyright (c) 2014 Fraunhofer Gesellschaft
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns carneades.web.modules.session.logic
  ^{:author "Sebastian Kaiser"
    :doc "Logic of session module"}
  (:require [sandbar.stateful-session :refer :all]
            [ring.middleware.session.cookie :refer :all]))

(defn session-put-language [lang]
  (if (nil? lang)
    (when (nil? (session-get :language)) (session-put! :language "en"))
    (when-not (= lang (session-get :language))
      (session-put! :language lang))))
