;;; Copyright (c) 2012 Fraunhofer Gesellschaft
;;; Licensed under the EUPL V.1.1

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

