;;; Copyright (c) 2013 Fraunhofer Gesellschaft
;;; Licensed under the EUPL V.1.1

(ns ^{:doc "A table with selectable elements."}
  carneades.analysis.web.views.selectable-table
  (:use [jayq.core :only [$ inner attr append]]))

(def state (atom {:selected nil}))

(defn- on-selection
  [event]
  (let [input ($ (.-target event))
        id (.-id (.-target event))
        checked (attr input "checked")]
    (if checked
      (swap! state assoc-in [:selected] id)
      (swap! state assoc-in [:selected] nil))))

(defn attach
  [selector]
  (doseq [input (.find ($ selector) "input[type=radio]")]
    (.change ($ input) on-selection)))

(defn selection
  []
  (:selected (deref state)))
