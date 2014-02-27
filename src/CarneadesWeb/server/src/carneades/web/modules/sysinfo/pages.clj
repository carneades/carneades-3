;; Copyright (c) 2012 Fraunhofer Gesellschaft
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns carneades.web.modules.polican.views.pages
  (:use [hiccup core page-helpers])
  (:require [net.cgrand.enlive-html :as html]
            [carneades.config.config :as config]))

(html/deftemplate index "carneades/public/index.html" [])

(defn render [t]
      (apply str t))

(defn index-page []
  (render (index)))

(defn config-page []
  (html5
   [:body
    (str "<hr>configfilename =<br><br> " config/configfilename "<br><br>"
         "<hr>properties = <br>"
         (apply str (sort (map (fn [[k v]] (str "<br> " k "=" v)) config/properties)))
         "<hr><br>java properties = <br>"
         (apply str (sort (map (fn [[k v]] (str "<br><br> " k "=" v)) (System/getProperties)))))]))
