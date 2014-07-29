;; Copyright (c) 2014 Fraunhofer Gesellschaft
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns ^{:doc "Argument Graph Validation"}
  carneades.engine.validation
  (:require [carneades.engine.argument :as a]
            [carneades.engine.argument-graph :as ag]
            [carneades.engine.theory :as t]))

(def cq-types 
  [:no-scheme          ; info: none
   :unknown-scheme     ; info: the scheme
   :no-role            ; info: URNs of the statements of the premises with no roles
   :missing-premises   ; info: the roles of the missing premises
   :unknown-premises   ; info: the roles not specified by the scheme
   :missing-exceptions ; info: the roles of the missing exceptions
   ])

(defrecord CQ  ; Critical Questions
   [type       ; keyword in cq-types
    argnode    ; URN symbol identifying the criticized argument node
    info       ; additional information, depending on the CQ type 
    ]) 

(defn- no-scheme
  "ArgumentNode -> (Seq CQ)
   Check whether an argumentation scheme has been specified for the node.
   If a scheme has been specified, return an empty vector. If not return 
   a singleton vector with a CQ describing this problem."
  [node]
  (when (nil? (:scheme node))
    [(CQ. :no-scheme (:id node) nil)]))

(defn- unknown-scheme
  "ArgumentNode (Map Symbol Scheme) -> (Seq CQ)
   If the argument node has a scheme, check if it is one of the 
   schemes in the map. If it is, return an empty vector, otherwise
   return a singelton vector with a CQ describing the problem."
  [node index]
  (let [scheme (:scheme node)]
    (cond (nil? scheme) []
          (and (list? scheme)
               (>= (count scheme) 1)
               (contains? index (first scheme))) []
               :else [(CQ. :unknown-scheme (:id node) scheme)])))

(defn- no-role
  "ArgumentNode -> (Seq CQ)
  Check whether a role has been specified for every premise of an argument node.
  If so, returns nil, otherwise return a singleton vector with a 
  CQ listing the URNs of the statements of the premises with no roles."
  [node]
  (let [stmts (map :statement 
                   (filter #(empty? (:role %)) 
                           (:premises node)))]
    (when stmts
      [(CQ. :no-role (:id node) stmts)])))

(defn- missing-premises
  "ArgumentNode (Map Symbol Scheme) -> (Seq CQ)
  Check whether an argument node is missing any premises specified
  in the scheme of the argument node. If not, return an empty vector.
  If so, return a singleton vector with a CQ listing the roles of the 
  missing premises."
  [node index]
  (let [scheme (:scheme node)]
    (cond (and (list? scheme)
               (>= (count scheme) 1)
               (contains? index (first scheme)))
          (let [missing (->> (:premises (get index (first scheme)))
                             (filter (fn [p] (not-any? (fn [r] (= r (:role p)))
                                                       (map :role (:premises node))))))]
            (when (not (empty? missing))
              [(CQ. :missing-premises (:id node) missing)]))
          :else [])))

(defn- unknown-premises
  "ArgumentNode (Map Symbol Scheme) -> (Seq CQ)
   Check whether any of the premises of an argument node have
   a role not specified in the scheme of the argument node. If not,
   return an empty vector. If so, return a singleton vector with 
   a CQ listing the roles not specified by the scheme."
  [node index]
  (let [scheme (:scheme node)]
    (cond (and (list? scheme)
               (>= (count scheme) 1)
               (contains? index (first scheme)))
          (let [unknown (->> (map :role (:premises node))
                             (filter (fn [r] 
                                       (not-any? 
                                        (fn [p] (= r (:role p)))
                                        (:premises (get index (first scheme)))))))]
            (when (not (empty? unknown))
              [(CQ. :unknown-premises (:id node) unknown)]
            :else [])))))

(defn- missing-exceptions 
  "ArgumentNode (Map Symbol Scheme) ArgumentGraph -> (Seq CQ)
   Check whether any of exceptions of the argument scheme of the
   argument node have not been used to undercut the argument node
   in the ArgumentGraph. If not, return an empty vector.  Otherwise
   return a singleton vector with a CQ listing the roles of the missing
   exceptions."
  [node index g]
  (let [scheme (:scheme node)
        exception-roles (when (and (list? scheme)
                                 (>= (count scheme) 1)
                                 (contains? index (first scheme)))
                          (map :role (:exceptions (get index (first scheme)))))
        undercutter-roles (map :role (ag/undercutters g node))
        missing (->> exception-roles
                     (filter (fn [r1] (not-any? (fn [r2] (= r1 r2))
                                                undercutter-roles))))]
    (when missing 
      [(CQ. :missing-exceptions (:id node) missing)])))

(defn validate-argument-node
  "ArgumentNode ArgumentGraph Theory -> (Seq CQ)"
  [node graph theory]
  (let [index (t/create-scheme-id-index theory)]
    (concat (no-scheme node)
            (unknown-scheme node index)
            (no-role node)
            (missing-premises node index)
            (unknown-premises node index)
            (missing-exceptions node index graph))))

(defn validate-argument-graph 
  "ArgumentGraph Theory -> (Seq CQ)"
  [graph theory]
  (mapcat #(validate-argument-node % graph theory)
          (vals (:argument-nodes graph))))




