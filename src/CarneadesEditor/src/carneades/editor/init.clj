;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.editor.init
  (:use clojure.contrib.def
        carneades.editor.utils.swing))

;;; import this file first to ensure this code is executed
;;; before any other

(defvar- *runonce*
  (do
    (System/setProperty "com.apple.mrj.application.apple.menu.about.name"
                        "Carneades Editor")
    ;; works only with native Mac L&F:
    ;; (System/setProperty "apple.laf.useScreenMenuBar" "true")
    (set-look-and-feel "Nimbus")))
