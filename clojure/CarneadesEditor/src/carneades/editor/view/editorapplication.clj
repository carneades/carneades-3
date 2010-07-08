(ns carneades.editor.view.editorapplication
  ;; tests
  (:use
   clojure.contrib.def
   carneades.engine.statement ;; just to test
   carneades.ui.diagram.jgraphviewer)
  (:import java.awt.EventQueue
           (javax.swing UIManager JFrame JInternalFrame)
           carneades.editor.uicomponents.EditorApplicationView))

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

;; this will be defined in an interface with Clojure 1.2 :

(defn display-graph [ag]
  (let [component (create-graph-component ag statement-formatted)]
    (.add *mapPanel* component)
    (.pack *mapPanel*)
    ;; (.pack *mainPanel*)
    ;; (.. *frame* getContentPane (add component))
    )
  (prn "display graph"))
