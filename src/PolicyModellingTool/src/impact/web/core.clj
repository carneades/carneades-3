(ns impact.web.core
  (:use clojure.pprint
        carneades.engine.dublin-core
        carneades.database.import
        carneades.engine.uuid)
  (:import java.io.File)
  (:require [carneades.database.db :as db]))

(def ^{:dynamic true} *debug* true)


;; (def swank-con swank.core.connection/*current-connection*)

;; (defmacro break []
;;   `(binding [swank.core.connection/*current-connection* swank-con]
;;      (swank.core/break)))

(defn store-ag
  "Stores ag into a LKIF and returns the dbname"
  [ag]
 (let [dbname (str "policymodellingtool-" (make-uuid))
        ;; TODO: changes the pass
        root "root"
        passwd "pw1"
        db (db/make-database-connection dbname root passwd)]
    (prn "dbname =" dbname)
    (db/create-argument-database dbname root passwd (make-metadata))
     (import-from-argument-graph db ag true)
     dbname))
