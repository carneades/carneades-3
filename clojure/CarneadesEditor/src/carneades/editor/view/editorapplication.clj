(ns carneades.editor.view.editorapplication
  (:use clojure.contrib.def
        carneades.mapcomponent.map)
  (:import java.awt.EventQueue
           (javax.swing UIManager JFrame JInternalFrame)
           carneades.editor.uicomponents.EditorApplicationView)
  (:gen-class))

(defprotocol View
  (display-graph [this ag stmt-fmt] "take the graph and a function that format 
                                     a statement")
  (ask-lkif-file-to-open [this]))


(defvar- *frame* EditorApplicationView/instance)
(defvar- *mapPanel* EditorApplicationView/mapPanel)

(defn- set-native-look-and-feel []
  (try
   (UIManager/setLookAndFeel (UIManager/getSystemLookAndFeelClassName))
   (catch Exception e)))

(defn launch-application [& args]
  (set-native-look-and-feel)
  (EventQueue/invokeLater
   (proxy [Runnable] []
     (run []
          (.setVisible *frame* true)))))

(deftype SwingView [] View
  (display-graph
   [this ag stmt-fmt]
   (let [component (create-graph-component ag stmt-fmt)]
     (.add *mapPanel* component)
     (.pack *mapPanel*)
     ;; (.pack *mainPanel*)
     ;; (.. *frame* getContentPane (add component))
     ))
  (ask-lkif-file-to-open [this] (print "ask lkif file")))

