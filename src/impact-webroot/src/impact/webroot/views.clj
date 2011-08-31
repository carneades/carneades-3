(ns impact.webroot.views
  (:use [hiccup core page-helpers]))

(defn index-page []
  (html5
    [:head
      [:title "Policy Impact"]
      (include-css "/css/style.css")]
    [:body
     [:h1 "Demo applications available: "]
     [:a {:href "/PolicyModellingTool"} "Policy Modelling and Analysis"]
     [:br]
     [:a {:href "/CarneadesWebGUI"} "LKIF visualization"]]))
