;; Copyright (c) 2014 Fraunhofer Gesellschaft
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns ^{doc "Create a subgraph from an argument graph."}
  carneades.engine.subag
  (:require [carneades.engine.argument-graph]
            [clojure.set :as set]
            [taoensso.timbre :as timbre :refer [debug info]]))

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

(defn get-nodesids
  "Return the nodes ids of the subag."
  [ag nid depth]
  (if-let [snode ((:statement-nodes ag) nid)]
    (explore-stmt ag depth (:id snode))
    (if-let [anode ((:argument-nodes ag) nid)]
      (conj (explore-arg ag (dec depth) (:id anode))
            (:conclusion anode))
      (throw (ex-info (str "Invalid id" nid) {:nid nid})))))

(defn subag
  "Return a subgraph from the ag for a particular node id."
  [ag nid depth]
  (let [nodesids (get-nodesids ag nid depth)
        ag (update-in ag [:statement-nodes] select-keys nodesids)
        ag (update-in ag [:argument-nodes] select-keys nodesids)
        ag (cut-arguments ag)
        ag (trim-language ag)]
    ag))
