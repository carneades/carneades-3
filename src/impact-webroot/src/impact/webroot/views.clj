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
     [:a {:href "/PolicyModellingTool2"} "Policy Modelling and Analysis (Clojure)"]
     [:br]
     [:a {:href "/CarneadesWebMappingTool"} "LKIF visualization (Clojure)"]]))
