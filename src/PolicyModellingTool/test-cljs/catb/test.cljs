;;; Copyright (c) 2012 Fraunhofer Gesellschaft
;;; Licensed under the EUPL V.1.1

(ns catb.test
  (:require [catb.test.navigation :as navigatop,]))

(def success 0)

(defn ^:export run []
  (.log js/console "Example test started.")
  (navigation/run)
  success)
