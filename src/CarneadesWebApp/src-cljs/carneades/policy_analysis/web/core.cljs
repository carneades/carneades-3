;;; Copyright (c) 2012 Fraunhofer Gesellschaft
;;; Licensed under the EUPL V.1.1

(ns carneades.policy-analysis.web.core
  (:require [clojure.string :as str]))

;; (defn clj->js
;;   "Recursively transforms ClojureScript maps into Javascript objects,
;;    other ClojureScript colls into JavaScript arrays, and ClojureScript
;;    keywords into JavaScript strings where '-' characters are replaced
;;    by '_'.

;;    Borrowed and updated from mmcgrana."
;;   [x]
;;   (cond
;;     (string? x) x
;;     (keyword? x) (str/replace (name x) "-" "_")
;;     (map? x) (.-strobj (reduce (fn [m [k v]]
;;                (assoc m (clj->js k) (clj->js v))) {} x))
;;     (coll? x) (apply array (map clj->js x))
;;     :else x))

;; (defn log
;;   [stuff & more]
;;   (.log js/console stuff)
;;   (doseq [x more]
;;     (.log js/console x)))
