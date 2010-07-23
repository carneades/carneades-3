(ns carneades.editor.view.tabs
  (:use clojure.contrib.def)
  (:import (java.awt EventQueue event.MouseListener Dimension FlowLayout)
           (javax.swing UIManager JTabbedPane JLabel JButton JFrame JPanel
                        JInternalFrame
                        JFileChooser
                        filechooser.FileFilter
                        SwingUtilities
                        BorderFactory
                        tree.DefaultMutableTreeNode
                        tree.TreeSelectionModel)
           (carneades.editor.uicomponents EditorApplicationView)))

(defvar *mapPanel* EditorApplicationView/mapPanel)

(defvar- *tabPopupMenu* EditorApplicationView/tabPopupMenu)

(defn- show-popupmenu [event]
  (when (.isPopupTrigger event)
    (.show *tabPopupMenu*
           (.getComponent event)
           (.getX event)
           (.getY event))))

(deftype TabMouseMenuListener [] MouseListener
  (mouseClicked [this event])
  (mouseEntered [this event])
  (mouseExited [this event])
  (mousePressed [this event]
                (prn "left click")
                (let [tabpanel (.getSource event)]
                  (prn "tabpanel")
                  (.dispatchEvent *mapPanel*
                                 (SwingUtilities/convertMouseEvent
                                  tabpanel event *mapPanel*))))
  (mouseReleased [this event]
                 (prn "left click released")))

(defvar- *tabMouseListener* (TabMouseMenuListener.))

(defn create-tabcomponent [title]
  (let [tabcomponent (JPanel.)
        label (JLabel. title)]
    (.setOpaque tabcomponent false)
    (.setFocusable label false)
    (.setComponentPopupMenu label *tabPopupMenu*)
    (.setBorder tabcomponent (BorderFactory/createEmptyBorder 5 5 5 5))
    (.setLayout tabcomponent (FlowLayout. FlowLayout/LEFT 0 0))
    (.add tabcomponent label)
    (.addMouseListener label *tabMouseListener*)
    tabcomponent))

(defn get-tabtitle [ag]
  (format "%s - %s" (:id ag) (:title ag)))

(defn get-tab [tabpanel title]
  "returns the index of the tab titled title or nil if it does not exist"
  (let [nbtabs (.getTabCount tabpanel)]
    (loop [n (dec nbtabs)]
      (if (neg? n)
        nil
        (if (= (.getTitleAt tabpanel n) title)
          n
          (recur (dec n)))))))

(defvar- *components-to-ags* (atom {}) "components -> [path graphid]")
(defvar- *ags-to-components* (atom {}) "[path id] -> component")

(defn add-component [component path id]
  (swap! *components-to-ags* assoc component [path id])
  (swap! *ags-to-components* assoc [path id] component))

(defn get-graphinfo [component]
  (get (deref *components-to-ags*) component))

(defn get-component [path id]
  (get (deref *ags-to-components*) [path id]))

(defn remove-component [component]
  (let [info (get-graphinfo component)] 
    (swap! *components-to-ags* dissoc component)
    (swap! *ags-to-components* dissoc info)))

(defn tabs-empty? []
  "true if no tabs"
  (empty? (deref *components-to-ags*)))
