;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns ^{:doc "Implementation of the About Box."}
  carneades.editor.view.dialogs.aboutbox
  (:use clojure.contrib.def
        clojure.contrib.swing-utils
        clojure.contrib.javadoc.browse)
  (:import (carneades.editor.uicomponents CarneadesAboutBox)))

(defvar- *url* "http://carneades.berlios.de/")

(defn show-about-box [parent]
  (let [dialog (CarneadesAboutBox. parent true)
        urlbutton (.urlButton dialog)]
    (add-action-listener (.closeButton dialog)
                         (fn [event]
                           (.dispose dialog)))
    (add-action-listener urlbutton
                         (fn [event]
                           (open-url-in-browser *url*)))
    (.setLocationRelativeTo dialog parent)
    (.setVisible dialog true)))

