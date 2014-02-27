;; Copyright (c) 2014 Fraunhofer Gesellschaft
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns ^{:doc "See http://thinkrelevance.com/blog/2013/06/04/clojure-workflow-reloaded"}
  user
  (:require [clojure.tools.namespace.repl :refer [refresh refresh-all]]
            [carneades.web.system :as system]))


(def system nil)

(defn init
  "Constructs the current development system."
  []
  (alter-var-root #'system
                  (constantly system/system)))

(defn start
  "Starts the current development system."
  []
  (alter-var-root #'system (system/start system)))

(defn stop
  "Shuts down and destroys the current development system."
  []
  (alter-var-root #'system 
                  (fn [s] (when s (system/stop s)))))

(defn go
  "Initializes the current development system and starts it running."
  []
  (init)
  (start)
  :ready)

(defn reset []
  (stop)
  (refresh :after 'user/go))
