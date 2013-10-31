;;; Copyright (c) 2012 Fraunhofer Gesellschaft
;;; Licensed under the EUPL V.1.1

(ns carneades.analysis.web.main
  (:require [carneades.analysis.web.system :as system]
            [clojure.java.browse :refer [browse-url]])
  (:gen-class))


(defn -main
  [& args]
  (system/start (system/system :mode :standalone))
  (browse-url "http://localhost:8080/carneades/#/home"))
