(ns carneades.web.views
  (:use [hiccup core page-helpers form-helpers]
        carneades.mapcomponent.export
        carneades.engine.lkif
        [carneades.engine.statement :only (statement-formatted)]
        [clojure.java.io :only (input-stream reader)]
        clojure.pprint)
  (:require [clojure.string :as s]))

(defn include-all-js []
  (html
   (map include-js ["/js/jquery-1.6.2.js"
                    "/js/jquery-ui-1.8.14.custom.min.js"
                    "/js/jquery.svg.js"
                    "/js/jquery.svgdom.js"
                    "/js/jquery.svganim.js"
                    "/js/jquery.mousewheel.js"
                    "/js/jquery.event.drag-2.0.js"
                    "/js/main.js"])))

(defn index-page []
  (html5
   (map include-css ["/css/custom-theme/jquery-ui-1.8.14.custom.css"
                     "/css/screen.css"
                     "/css/jquery.svg.css"])
   (include-all-js)
    [:head
      [:title "LKIF Argument Mapping Testbed"]]
    [:body
     [:div {:id "container"}
      [:div {:id "header" :class "ui-widget-header ui-corner-top"}
       [:h1 "LKIF Argument Mapping Testbed"]]
      [:div {:id "filechooser"}
       [:form {:action "/CarneadesWebMappingTool/files" :method "POST" :enctype "multipart/form-data" :target "lkif-source"}
        "LKIF to upload:"
        (file-upload "lkif-file")
        [:button  {:class "start" :type "submit"} "Upload"]]]
      [:div {:id "tabs"}
       (unordered-list [[:a {:href "#tabs-1"} "Graph"]
                        [:a {:href "#tabs-2"} "Source"]
                        ])
       [:div {:id "tabs-1"}
        [:div {:id "wrapper"}
         [:div {:id "graphbox" :class "ui-corner-left ui-corner-right"}]]
        [:div {:id "toolbar"}
         [:form {:class "ui-widget-content ui-corner-right"}
          [:p "Layout"]
          [:select {:id "Layout" :name "Options" :size 1}
           [:option "Radial"]
           [:option "Hierarchical"]]
          [:br]
          "Treeify"
          [:input {:id "Treeify" :type "checkbox" :name "Options" :value "treeify"}]
          [:br]
          [:input {:id "UpdateButton" :type "button" :name "Options" :value "Update"
                   :onclick "updateSVG()"}]]]
        [:div {:id "toolbar"}]]
       [:div {:id "tabs-2"}
        [:div {:id "extra"}
         [:iframe {:name "lkif-source" :id "lkif-source" :src "#" :onload "loadSVG()" :width 1200 :height 600}]]]
       ]
      [:div {:id "footer" :class "ui-widget-header ui-corner-bottom"}
       [:p "Fraunhofer FOKUS"]]
      [:div {:id "debug"}]]]))

(defn keywordify
  [h]
  (apply hash-map (flatten (map (fn [[k v]] [(keyword k) v]) h))))

(defn get-layout
  [layoutparam]
  (condp = (s/lower-case layoutparam)
    "radial" :radial
    "hierarchical" :hierarchical
    ;; default to radial
    :radial))

(defn output-svg
  [session params]
  (let [lkif (import-lkif (input-stream (.getBytes (:lkif-file session))))
        {:keys [layout treeify radius depth]} params;; (keywordify params)
        pa (merge {:layout (get-layout layout)}
                  (if (nil? treeify)
                    nil
                    {:treeify (Boolean/valueOf treeify)})
                  (if (nil? radius)
                    nil
                    {:radius (Integer/valueOf radius)})
                  (if (nil? depth)
                    nil
                    {:depth (Integer/valueOf depth)}))
        ;; _ (do (prn "pa =") (prn pa))
        svg (apply export-ag-os (first (:ags lkif)) statement-formatted (flatten (map identity pa)))]
    (apply str (line-seq (reader svg)))))

(defn view-file
  [session params]
  {:headers {"Content-Type" "text/xml;charset=UTF-8"}
   :body (output-svg session params)})

(defn view-session
  [session]
  (html5
   (str session)))

(defn display-lkif-src
  [content]
  (html5
   [:pre (escape-html content)]))

(defn upload-file
  [file session]
  ;; add the content of the file to the session
  (let [content (slurp (file :tempfile))]
   {:session (assoc session :lkif-file content)
    :body (display-lkif-src content)}))