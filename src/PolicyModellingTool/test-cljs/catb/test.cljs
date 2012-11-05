(ns catb.test
  (:require [catb.test.navigation :as navigatop,]))

(def success 0)

(defn ^:export run []
  (.log js/console "Example test started.")
  (navigation/run)
  success)
