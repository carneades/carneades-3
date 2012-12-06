;;; Copyright (c) 2012 Fraunhofer Gesellschaft
;;; Licensed under the EUPL V.1.1

(ns catb.playground
  (:require [catb.backbone.core :as bb]
            [catb.icanhaz.core :as ich]
            [catb.template :as tp])
  (:require-macros [catb.backbone.macros :as bb]))
 
(def $ (js* "$"))

(bb/defview SctSummary
  :className "sct-summary"
  :events {"click" "do_stuff"}
  :render ([]
             (bb/html this "<div>MY VIEW FROM CLOJURE</view>"))
  :do_stuff ([]
               (.log js/console (bb/get-html this))
               (js/alert "hello")))
 
(defn test-icanhaz
  []
  (tp/get :sct_summary {:sct_summary_text "Helloz"}))