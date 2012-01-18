;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns ^{:doc "Read and save properties from the properties file"}
    carneades.config.reader
  (:use clojure.java.io)
  (:import java.io.File))

(def configfilename
     (str (System/getProperty "user.home")
          File/separator
          ".carneades.properties"))

(defn read-bundled-properties
  "Read properties from filename. Filename is searched in the classpath"
  ;; see also clojure.contrib.java-utils
  [filename]
  (doto (java.util.Properties.)
    (.load (-> (Thread/currentThread)
               (.getContextClassLoader)
               (.getResourceAsStream filename)))))

(defn read-properties
  "Reads the properties contained in pathname and returns a map.
   throws java.io.FileNotFoundException"
  [pathname]
  (into {}
        (let [properties (java.util.Properties.)]
         (.load properties (input-stream pathname))
         properties)))

(defn save-properties
  "Saves the map to a property file."
  ([h pathname]
     (save-properties h pathname "Carneades properties file"))
  ([h pathname comments]
     (let [properties (java.util.Properties.)]
       (doseq [[k v] h]
         (.setProperty properties k v))
       (.store properties (output-stream pathname ) comments))))

(def properties
     (try
       (read-properties configfilename)
       (catch Exception _ {})))
