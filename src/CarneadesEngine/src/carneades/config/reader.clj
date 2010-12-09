;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.config.reader
  (:use clojure.contrib.def
        clojure.java.io))

(def *configfilename* "carneades/carneades.properties")

(defn read-bundled-properties
  "Read properties from filename. Filename is searched in the classpath"
  ;; see also clojure.contrib.java-utils
  [filename]
  (doto (java.util.Properties.)
    (.load (-> (Thread/currentThread)
               (.getContextClassLoader)
               (.getResourceAsStream filename)))))

(defvar- #^java.util.Properties *properties*
  (read-bundled-properties *configfilename*))

(defn configvalue [s]
  "Returns the value of the configuration property named s"
  (.getProperty *properties* s))

(defn read-properties [pathname]
  "reads the properties contained in pathname and returns a hashmap.
   throws java.io.FileNotFoundException"
  (into {}
        (let [properties (java.util.Properties.)]
         (.load properties (input-stream pathname))
         properties)))

(defn save-properties
  ([h pathname]
     (save-properties h pathname "Carneades properties file"))
  ([h pathname comments]
     (let [properties (java.util.Properties.)]
       (doseq [[k v] h]
         (.setProperty properties k v))
       (.store properties (output-stream pathname ) comments))))
