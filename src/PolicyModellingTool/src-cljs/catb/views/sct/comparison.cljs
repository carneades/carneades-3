(ns catb.views.sct.comparison
  (:use [catb.views.core :only [template json]]
   [jayq.core :only [$ css inner attr val]]
   [jayq.util :only [log clj->js]])
    (:require [catb.backbone.core :as bb])
    (:require-macros [catb.backbone.macros :as bb]))

(bb/defview Comparison
  :className "sct-comparison"
  :events {"click .save" :save-score}
  :render
  ([]
     (template this :sct-comparison {})
     )

  :save-score
  ([]
     ))