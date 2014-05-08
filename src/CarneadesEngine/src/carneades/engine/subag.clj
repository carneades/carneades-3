;; Copyright (c) 2014 Fraunhofer Gesellschaft
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns ^{doc "Create a subgraph from an argument graph."}
  carneades.engine.subag
  (:require [carneades.engine.argument-graph]
            [clojure.set :as set]
            [taoensso.timbre :as timbre :refer [debug info]]))

(defn- get-rootid
  [ag nid]
  (if-let [s ((:statement-nodes ag) nid)]
    (:id s)
    (if-let  [a ((:argument-nodes ag) nid)]
      ((:statement-nodes ag) (:conclusion a))
      (throw (ex-info (str "Invalid id " nid) {:nid nid})))))

(declare explore-stmt)

(defn explore-arg
  [ag depth aid]
  (let [anode ((:argument-nodes ag) aid)]
    (apply set/union
           #{aid}
           (map (comp (partial explore-stmt ag depth)
                      :statement)
                (:premises anode)))))

(defn explore-stmt
  [ag depth sid]
  (if-let [snode ((:statement-nodes ag) sid)]
    (if (zero? depth)
      #{sid}
      (apply set/union
             #{sid}
             (concat
                     (map (partial explore-arg ag (dec depth)) (:pro snode))
                     (map (partial explore-arg ag (dec depth)) (:con snode)))))
    (throw (ex-info (str "Invalid id " sid) {:sid sid}))))

(defn cut-arguments
  "Remove references to non-existing arguments."
  [ag]
  (let [argids (set (keys (:argument-nodes ag)))
        cut (fn [ag sid]
              (let [snode ((:statement-nodes ag) sid)]
                (update-in ag [:statement-nodes sid]
                           assoc
                           :pro (filter argids (:pro snode))
                           :con (filter argids (:con snode))
                           :premise-of (filter argids (:con snode)))))]
    (reduce cut ag (keys (:statement-nodes ag)))))

(defn trim-language
  "Remove references to non-existing nodes."
  [ag]
  (let [nodeids (concat (keys (:statement-nodes ag))
                        (keys (:argument-nodes ag)))]
    (update-in ag [:language] select-keys nodeids)))

(defn subag
  "Return a subgraph from the ag for a particular node id."
  [ag nid depth]
  (let [rootid (get-rootid ag nid)
        nodes (explore-stmt ag depth rootid)
        ag (update-in ag [:statement-nodes] select-keys nodes)
        ag (update-in ag [:argument-nodes] select-keys nodes)
        ag (cut-arguments ag)
        ag (trim-language ag)]
    ag))
