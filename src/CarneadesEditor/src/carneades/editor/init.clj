;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.editor.init
  (:use clojure.contrib.def)
  ;; use seems to cause a side-effect? making
  ;; the setted property ineffective, so we use require:
  (:require [carneades.editor.utils.swing :as swing]))

;;; import this file first to ensure this code is executed
;;; before any other

(defvar- *runonce*
  (do
    (System/setProperty "com.apple.mrj.application.apple.menu.about.name"
                        "Carneades Editor")
    (swing/set-look-and-feel "Nimbus")
    ;; works only with native Mac L&F:
    ;; (System/setProperty "apple.laf.useScreenMenuBar" "true")
    ))
