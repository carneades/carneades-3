;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

;;; import this file first to ensure this code is executed
;;; before any other

;; this needs to be runned before the :use of swing
(System/setProperty "com.apple.mrj.application.apple.menu.about.name"
                    "Carneades Editor")

(ns ^{:doc "Include this namespace first to unsure operations below
            are executed before anything else."}
    carneades.editor.init
  (:use clojure.contrib.def
        carneades.editor.utils.swing))

(defvar- *runonce*
  (do
    ;; works only with native Mac L&F:
    ;; (System/setProperty "apple.laf.useScreenMenuBar" "true")    
    (set-look-and-feel "Nimbus")))
