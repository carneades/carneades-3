;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns ^{:doc "Utilities to split the implementation of a protocol into
            several files."}
  carneades.editor.utils.type
  (:use clojure.contrib.def
        [clojure.walk :only (postwalk-replace)]))

;; The two following functions are taken from the Penumbra project
;; with the permission of Zachary Tellman.
;;
;; http://github.com/ztellman/penumbra

(defn- transform-extend-arglists [protocol name arglists template]
  (list*
   `fn
   (map
    (fn [args]
      (let [template (postwalk-replace {'this (first args)} template)]
        (list
         (vec args)
         (list*
          (intern (symbol (namespace protocol)) name)
          template
          (next args)))))
    arglists)))

(defmacro auto-extend
  "Lets the application, which contains an implementation of a protocol, automatically extend that protocol."
  [type protocol template & explicit]
  (let [sigs (eval `(vals (:sigs ~protocol)))]
    (list
     `extend
     type
     protocol
     (merge
      (->> (map
            (fn [{name :name arglists :arglists}]
              [(keyword name)
               (transform-extend-arglists protocol name arglists template)])
            sigs)
           (apply concat)
           (apply hash-map))
      (apply hash-map explicit)))))

