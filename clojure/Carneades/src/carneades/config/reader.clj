;;; Carneades Argumentation Library and Tools.
;;; Copyright (C) 2010 Thomas F. Gordon, Fraunhofer FOKUS, Berlin
;;; 
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU Lesser General Public License version 3 (LGPL-3)
;;; as published by the Free Software Foundation.
;;; 
;;; This program is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for details.
;;; 
;;; You should have received a copy of the GNU Lesser General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

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
