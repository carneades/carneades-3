(ns impact.web.views
  (:use [hiccup core page-helpers])
  (:require [net.cgrand.enlive-html :as html]))

(html/deftemplate index "public/index.html" [])

(defn render [t]
      (apply str t))

(defn index-page []
  (render (index)))
