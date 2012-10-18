(ns catb.template
  (:require [catb.icanhaz.core :as ich])
  (:refer-clojure :exclude [get]))

(defn get
  [template variables]
  (ich/get template (merge (js->clj js/jQuery.i18n.map) variables)))
