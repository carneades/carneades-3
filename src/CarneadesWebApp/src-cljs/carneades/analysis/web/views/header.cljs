;;; Copyright (c) 2013 Fraunhofer Gesellschaft
;;; Licensed under the EUPL V.1.1

(ns carneades.analysis.web.views.header
  (:use [jayq.core :only [$ inner]]
        [jayq.util :only [log]]
        [carneades.analysis.web.i18n :only [i18n]])
  (:require [carneades.analysis.web.template :as tp]))

(defn get-title
  [title-or-key]
  (if (keyword? title-or-key)
    (i18n title-or-key)
    title-or-key))

(defn build-menu-item
  [item first last]
  (let [class (cond first "first"
                    last "last"
                    :else "")]
   (format "<li><a href=\"%s\" class=\"%s\">%s</a></li>"
           (:link item)
           class
           (i18n (keyword (:text item))))))

(defn build-menu
  [menu]
  (if (seq menu)
    (let [html (reduce (fn [html item]
                         (str html (build-menu-item item false false)))
                       ""
                       (rest (butlast menu)))]
      (str (build-menu-item (first menu) true false)
           html
           (build-menu-item (last menu) false true)))
    ""))

(defn show-menu
  [menu]
  (inner ($ ".section-menu ul") (build-menu menu)))

(defn build-title
  [title-data]
  (format "<a href=\"%s\">%s</a> "
          (:link title-data)
          (get-title (:text title-data))))

(defn attach-listeners
  [menu]
  (doseq [m menu]
    (when-let [on (:on m)]
      (.click ($ (format "a[href=\"%s\"]" (:link m))) on))))

(defn show
  "Title-data is a map with a :text and a :link keys. If text is a
string it is used directly, if it is a keyword the translation
associated to the keyword is used."
  ([title-data]
     (show title-data []))
  ([title-data menu]
     (let [title (build-title title-data)]
       (inner ($ ".topheader") (tp/get "header" {}))
       (inner ($ ".section-title") title)
       (show-menu menu)
       (attach-listeners menu)
       (js/PM.attach_lang_listener))))
