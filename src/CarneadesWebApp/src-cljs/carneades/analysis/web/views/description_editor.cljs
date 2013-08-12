;;; Copyright (c) 2013 Fraunhofer Gesellschaft
;;; Licensed under the EUPL V.1.1

(ns carneades.analysis.web.views.description-editor
  (:use [jayq.core :only [$ inner attr append]]
        [jayq.util :only [log]])
  (:refer-clojure :exclude [get])
  (:require [carneades.analysis.web.dispatch :as dispatch]))

(defn- initial-state
  []
  {:lang :en
   :value {}})

(def state (atom (initial-state)))

(defn update-state
  ;; assigns selected lang to the new selected language
  ;; and the actual description to the old one
  [state new-lang description]
  (let [previous-lang (:lang state)]
   (-> state
       (assoc :lang new-lang)
       (assoc-in [:value previous-lang] description))))

(defn get-lang
  [a]
  (let [href (.attr ($ a) "href")
        lang (subs href (- (count href) 2))]
    (keyword lang)))

(defn select-tab
  [lang]
  (doseq [li ($ ".description-editor > ul > li")]
    (let [li-el ($ li)]
     (if (= (get-lang (.find li-el "a")) lang)
       (do
         (.addClass li-el "ui-tabs-selected")
         (.addClass li-el "ui-state-active"))
       (do
         (.removeClass li-el "ui-tabs-selected")
         (.removeClass li-el "ui-state-active"))))))

(defn on-description-tab-changed
  [_ msg]
  (swap! state update-state (:lang msg) (:description msg))
  (let [s (deref state)]
    (.val ($ ".description") (get-in s [:value (:lang msg)] "")))
  (select-tab (:lang msg)))

(dispatch/react-to #{::description-tab-changed}
                   on-description-tab-changed)

(defn get
  []
  (let [{:keys [lang value]} (deref state)
        description (.val ($ ".description"))]
   (merge value
          {lang description})))

(defn- on-tab-selected
  [event ui]
  (let [tab (.-tab ui)
        lang (get-lang tab)]
    (dispatch/fire ::description-tab-changed
                   {:lang lang
                    :description (.val ($ ".description"))}))
  false)

(defn show
  [selector description]
  (reset! state (initial-state))
  (swap! state assoc-in [:value] description)
  (.val ($ ".description") (:en description))
  (.tabs ($ selector) (clj->js {:select on-tab-selected
                               :selected 0}))
  (.markItUp ($ (str selector " .description")) js/mySettings))
