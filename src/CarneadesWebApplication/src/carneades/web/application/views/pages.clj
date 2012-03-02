(ns carneades.web.application.views.pages
  (:require [net.cgrand.enlive-html :as html]))

(html/deftemplate index "public/indexwebapp.html" [])

(defn render [t]
  (apply str t))

(defn index-page []
  {:body (render (index))
   :headers {"Content-Type" "text/html; charset=utf-8"}})

