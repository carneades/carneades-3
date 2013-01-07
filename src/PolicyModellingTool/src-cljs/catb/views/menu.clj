;;; Copyright (c) 2012 Fraunhofer Gesellschaft
;;; Licensed under the EUPL V.1.1

(ns ^{:doc "Utilities to deal with the HTML menu"}
    catb.views.menu)

(defmacro with-item
  "Activates the menu item `item` and attachs the lang listener."
  ;; this could be a function but the usage of a macro here
  ;; makes the code look more declarative
  [item-selector & body]
  `(let [item-selector# ~item-selector]
     ~@body
     (js/PM.activate item-selector#)
     (js/PM.attach_lang_listener)))