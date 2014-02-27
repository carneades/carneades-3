;; Copyright (c) 2013 Fraunhofer Gesellschaft
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns carneades.web.info
  (:use [carneades.web.pack :only [argument-data pack-argument pack-statement]])
  (:require [carneades.database.argument-graph :as ag-db]))

(defn arg-info
  "Returns information concerning the argument and its context."
  [id]
  (let [arg (ag-db/read-argument (str id))
        arg (pack-argument arg)
        undercutters-data (doall (map argument-data (:undercutters arg)))
        rebuttals-data (doall (map argument-data (:rebuttals arg)))
        dependents-data (doall (map argument-data (:dependents arg)))]
    (assoc arg
      :undercutters-data undercutters-data
      :rebuttals-data rebuttals-data
      :dependents-data dependents-data)))

(defn stmt-info
  [id]
  (let [stmt (pack-statement (ag-db/read-statement (str id)))
        pro-data (doall (map argument-data (:pro stmt)))
        con-data (doall (map argument-data (:con stmt)))
        premise-of-data (doall (map argument-data (:premise-of stmt)))]
    (assoc stmt 
      :pro-data pro-data 
      :con-data con-data
      :premise-of-data premise-of-data)))
