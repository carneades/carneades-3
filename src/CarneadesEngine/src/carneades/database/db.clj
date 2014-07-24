;; Copyright (c) 2011-2013 Fraunhofer Gesellschaft
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;;

(ns ^{:doc "Utilities for interacting with databases."}
  carneades.database.db
  (:require [carneades.config.config :as config]
            [clojure.java.jdbc.deprecated :as jdbc]
            [carneades.engine.uuid :as uuid]
            [carneades.project.fs :as project])
  (:import java.io.File))

(defmacro with-db [db & body]
  `(jdbc/with-connection
     ~db
     (jdbc/transaction ~@body)))

(defmacro test-db
  "For testing and development.  Doesn't do any
   any error handling so don't use in production code."
  [db & body]
  `(jdbc/with-connection
           ~db
           (jdbc/transaction ~@body)))

;;; Databases

(def default-db-protocol "file")

(defn dbfilename
  "Returns the filename of a database."
  [project dbname]
  (str project/projects-directory "/" project "/databases/" dbname ".h2.db"))

;; (defn fetch-databases-names
;;   "Looks on the disk to find all existing databases. Returns their names"
;;   []
;;   (keep #(second (re-find #"(.*)\.h2\.db$" %))
;;         (map (memfn getName)
;;              (file-seq (clojure.java.io/file default-db-host)))))

(defn make-connection
  "Returns a map describing a database connection. Use the :create
true option to create a new database."
  ([project-name db-name username passwd & options]
     (let [options (apply hash-map options)
           db-protocol (:protocol options default-db-protocol) ;; "file|mem|tcp"
           db-directory (str project/projects-directory File/separator
                             project-name File/separator "databases") ;; "path|host:port"
           db-host (str db-protocol "://" db-directory "/" db-name)
           db-host (if (:create options)
                     db-host
                     ;; see http://www.h2database.com/html/features.html#database_only_if_exists
                     (str db-host ";IFEXISTS=TRUE"))
           ]
       {:classname   "org.h2.Driver"
        :subprotocol "h2"
        :subname db-host
        :user  username
        :password passwd})))

(defn make-copy
  "Makes a copy of the database and returns the copy's name"
  [project dbname username password]
  (let [script (with-db (make-connection project dbname username password)
                 (jdbc/with-query-results content ["script"] (doall (map :script content))))
        newdbname (uuid/make-uuid-str)]
    ;; TODO take new-username and new-password as arguments and remove old admin access
    (with-db (make-connection project
                              newdbname
                              username
                              password
                              :create true)
      (apply jdbc/do-commands script))
    newdbname))
