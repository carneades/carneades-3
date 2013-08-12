;;; Copyright (c) 2012 Fraunhofer Gesellschaft
;;; Licensed under the EUPL V.1.1

(ns carneades.analysis.web.backbone.core
  (:use [jayq.util :only [log]])
  (:refer-clojure :exclude [get-in]))

(defprotocol IHtml
  (html [this])
  (html [this content]))

(extend-type js/Backbone.View
  IHtml
  (html [this]
    (.html (.-$el this)))
  (html [this content]
    (.html (.-$el this) content)))

(defn get-in*
  "Retrieves a property within nested backbone models"
  [model properties]
  (loop [current model
         properties properties]
    (if (seq properties)
      (recur (.get current (name (first properties)))
             (rest properties))
      current)))

(defn get-in
  "Retrieves a property within nested backbone models.
   The property is converted to a Clojure object"
  [model properties]
  (js->clj (get-in* model properties)))

(defn new
  "Returns a new Backbone object."
  [type attributes]
  (new type (clj->js attributes)))

(defn new-model
  "Returns a new Backbone Model."
  [attributes]
  (js/Backbone.Model. (clj->js attributes)))

(defn render
  "Renders a view"
  [view]
  (.render view))

(defn save
  "Save a model"
  [model attributes options]
  (.save model (clj->js attributes) (clj->js options)))
