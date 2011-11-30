(ns carneades.web.application.views.pages
  (:require [net.cgrand.enlive-html :as html]))

(html/deftemplate index "public/index.xhtml" [])

(defn render [t]
  (apply str t))

(defn index-page []
  (render (index)))

