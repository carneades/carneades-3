;;; Copyright (c) 2013 Fraunhofer Gesellschaft
;;; Licensed under the EUPL V.1.1

(ns carneades.analysis.web.views.project
  (:use [jayq.core :only [$ inner]]
        [jayq.util :only [log]]
        [carneades.analysis.web.views.core :only [json]]
        [carneades.analysis.web.i18n :only [i18n]])
  (:require [carneades.analysis.web.template :as tp]
            [carneades.analysis.web.views.header :as header]))

(defn ^:export show
  [project]
  (let [proj (.get js/PM.projects project)
        pdata (json proj)]
    (header/show {:text (.-title pdata) :link (str "#/project/" project)}
                 [{:text :arguments
                   :link (format "#/arguments/outline/%s/%s" project "main")}
                  {:text :guidedtour
                   :link (format "#/tour/intro/%s" project)}
                  {:text :policies
                   :link (format "#/policies/introduction/%s" project)}])
    (js/PM.busy_cursor_on)
    (js/PM.load_project project)
    (js/PM.busy_cursor_off)
    (inner ($ ".content")
           (tp/get "project"
                   {:description
                    (js/PM.markdown_to_html (aget (.-description pdata) js/IMPACT.lang))}))))
