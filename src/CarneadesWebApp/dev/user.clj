(ns ^{:doc "See http://thinkrelevance.com/blog/2013/06/04/clojure-workflow-reloaded"}
  user
  (:require [clojure.tools.namespace.repl :refer [refresh refresh-all]]
            [carneades.analysis.web.system :as system]))


(def system nil)

(defn init
  "Constructs the current development system."
  []
  (alter-var-root #'system
                  (constantly (system/system))))

(defn start
  "Starts the current development system."
  []
  (alter-var-root #'system system/start))

(defn stop
  "Shuts down and destroys the current development system."
  []
  (alter-var-root #'system
                  (fn [s] (when s (system/stop s)))))

(defn go
  "Initializes the current development system and starts it running."
  []
  (init)
  (start))

(defn reset []
  (stop)
  (refresh :after 'user/go))
