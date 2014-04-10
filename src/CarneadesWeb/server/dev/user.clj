;; Copyright (c) 2014 Fraunhofer Gesellschaft
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns
  user
  (:require [carneades.web.repl :as repl]))

;; Loosely inspired from http://thinkrelevance.com/blog/2013/06/04/clojure-workflow-reloaded
;; refresh would not work here since we are using gen-classes

(defn start
  "Starts the current development system."
  []
  (repl/start-server repl/system))

(defn stop
  "Shuts down and destroys the current development system."
  []
  (repl/stop-server repl/system))


(def go start)
