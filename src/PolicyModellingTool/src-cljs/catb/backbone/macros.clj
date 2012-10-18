(ns catb.backbone.macros
  (:use [clojure.pprint :only [pprint]])
  (:require [clojure.string :as str]))

(defn keyword->js
  [key]
  (str/replace (name key) "-" "_"))

(defn js-objectify
  "Takes a Clojure map, possibly nested, and returns the ClojureScript
 code that converts it to a JS object."
  [m]
  (letfn [(objectify [[[k v] & nxt]]
            (if-not (nil? k)
              (cons [(if (keyword? k)
                       (keyword->js k)
                       k)
                     (if (map? v)
                       (js-objectify v)
                       v)]
                    (objectify nxt))
              ()))]
    (cons 'js-obj (apply concat (objectify (seq m))))))

(defn add-fn-this-as
  "Iterates over a collection and changes item of the form ([x y z] ...)
   to (fn [x y z] (this-as this ...))"
  [coll]
  (let [[fndecl & nxt] coll]
    (cond (nil? fndecl) nil
          (and (list? fndecl)
               (vector? (first fndecl)))
          (let [[vardecl & body] fndecl]
            (cons `(~'fn ~vardecl (~'this-as ~'this ~@body ~'this))
                  (add-fn-this-as nxt)))
      :else (cons fndecl (add-fn-this-as nxt)))))

(defn events-handlers-names->js
  [events]
  (reduce-kv (fn [acc k v]
               (assoc acc k (keyword->js v)))
             {}
             events))

(defmacro defview
  "Create a backbone view. Usage example:
   (bb/defview SctSummary
     :className \"sct-summary\"
     :events {\"click\" :do-stuff}
     :render ([]
               )
      :do-stuff ([]
                 (js/alert \"hello\")))"
  [name & body]
  (let [hashed-body (apply hash-map body)
        hashed-body (update-in hashed-body [:events]
                               events-handlers-names->js)
        objectified (js-objectify hashed-body)
        fnified (add-fn-this-as objectified)]
   `(def ~name
      (.extend js/Backbone.View ~fnified))))

(defmacro with-attrs
  "Creates an implicit let that binds model attributes.
   Usage: (bb/with-attrs [:statements :arguments] ...)"
  [attributes & body]
  (let [letdefs (mapcat
                 (fn [attribute]
                   (let [name (name attribute)
                         varname (symbol name)]
                        `[~varname (~'.get ~'model ~name)]))
                     attributes)]
   `(let [~'model (aget ~'this "model")
          ~@letdefs]
     ~@body)))