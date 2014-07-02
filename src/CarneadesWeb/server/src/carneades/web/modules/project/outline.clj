;; Copyright (c) 2013 Fraunhofer Gesellschaft
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns carneades.web.modules.project.outline
  (:use (carneades.engine statement argument)
        carneades.web.modules.project.pack
        carneades.database.argument-graph))

(defn create-outline-helper
  [n depth]
  (cond (= depth 1)
        [n []]

        (statement? n)
        (let [procon (concat (map argument-data (:pro n)) (map argument-data (:con n)))]
          [n (vec (map #(create-outline-helper % (dec depth)) procon))])

        (argument? n)
        (let [premises (map (comp pack-statement :statement) (:premises n))]
          [n (vec (map #(create-outline-helper % (dec depth)) premises))])))

(defn create-outline
  [data depth]
  [:root (vec (map #(create-outline-helper % depth) (map pack-statement data)))])
