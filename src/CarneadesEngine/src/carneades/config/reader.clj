;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.config.reader
  (:use clojure.contrib.def))

(def *configfilename* "carneades/carneades.properties")

(defn- read-properties
  "Read properties from filename. Filename is searched in the classpath"
  ;; see also clojure.contrib.java-utils
  [filename]
  (doto (java.util.Properties.)
    (.load (-> (Thread/currentThread)
               (.getContextClassLoader)
               (.getResourceAsStream *configfilename*)))))

(defvar- #^java.util.Properties *properties* (read-properties *configfilename*))

(defn configvalue [s]
  "Returns the value of the configuration property named s"
  (.getProperty *properties* s))
