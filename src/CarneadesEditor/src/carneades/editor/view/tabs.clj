;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.editor.view.tabs
  (:use clojure.contrib.def
        clojure.contrib.swing-utils)
  (:import (java.awt EventQueue event.MouseListener Dimension FlowLayout)
           (javax.swing UIManager JTabbedPane JLabel JButton JFrame JPanel
                        ImageIcon
                        JInternalFrame
                        JFileChooser
                        filechooser.FileFilter
                        SwingUtilities
                        BorderFactory
                        tree.DefaultMutableTreeNode
                        tree.TreeSelectionModel)
           (carneades.editor.uicomponents EditorApplicationView)))

(defvar- *viewinstance* (EditorApplicationView/instance))

(defvar *mapPanel* (.mapPanel *viewinstance*))

(defvar- *closebutton-url* "carneades/editor/view/close-button.png")
(defvar- *closebutton-rollover-url*
  "carneades/editor/view/close-button-rollover.png")

(defvar- *close-button-listeners* (atom ()))

(defvar- *components-to-ags* (ref {}) "components -> [path graphid]")
(defvar- *ags-to-components* (ref {}) "[path id] -> component")

(defn init-tabs []
  (.setTabLayoutPolicy *mapPanel* JTabbedPane/SCROLL_TAB_LAYOUT))

(defn get-graphinfo [component]
  (get (deref *components-to-ags*) component))

(defn register-close-button-listener [l args]
  (swap! *close-button-listeners* conj {:listener l :args args}))

(defn graphinfo-being-closed [event]
  "returns [path id]"
  (let [button (.getSource event)
        tabcomponent (.getParent button)
        idx (.indexOfTabComponent *mapPanel* tabcomponent)
        component (.getComponentAt *mapPanel* idx)]
    (get-graphinfo component)))

(defn create-close-button []
     (let [closebutton (JButton.)]
       (doto closebutton
         (.setBorder nil)
         (.setIcon (ImageIcon. (ClassLoader/getSystemResource
                                *closebutton-url*)))
         (.setRolloverIcon (ImageIcon. (ClassLoader/getSystemResource
                     *closebutton-rollover-url*)))
         ;; (.setFocusable false)
         (.setContentAreaFilled false)
         (.setRolloverEnabled true))
       (doseq [{:keys [listener args]} (deref *close-button-listeners*)]
         (apply add-action-listener closebutton listener args))
       closebutton))

(defn create-tabcomponent [title]
  (let [tabcomponent (JPanel.)
        label (JLabel. title)
        closebutton (create-close-button)]
    (.setOpaque tabcomponent false)
    (.setFocusable label false)
    (.setFocusable tabcomponent false)
    ;; (.setComponentPopupMenu label *tabPopupMenu*)
    (.setBorder tabcomponent (BorderFactory/createEmptyBorder 5 5 5 5))
    (.setLayout tabcomponent (FlowLayout. FlowLayout/LEFT 0 0))
    (.add tabcomponent label)
    (.add tabcomponent closebutton)
    ;; (.addMouseListener label *tabMouseListener*)
    tabcomponent))

(defn get-tabtitle [ag]
  (format "%s - %s " (:id ag) (:title ag)))

;; (defn get-tab [tabpanel title]
;;   "returns the index of the tab titled title or nil if it does not exist"
;;   (let [nbtabs (.getTabCount tabpanel)]
;;     (loop [n (dec nbtabs)]
;;       (if (neg? n)
;;         nil
;;         (if (= (.getTitleAt tabpanel n) title)
;;           n
;;           (recur (dec n)))))))

(defn add-component [component path id]
  (dosync
   (alter *components-to-ags* assoc component [path id])
   (alter *ags-to-components* assoc [path id] component)))

(defn get-component [path id]
  (get (deref *ags-to-components*) [path id]))

(defn remove-component [component]
  (let [info (get-graphinfo component)]
    (dosync
     (alter *components-to-ags* dissoc component)
     (alter *ags-to-components* dissoc info))))

(defn tabs-empty? []
  "true if no tabs"
  (empty? (deref *components-to-ags*)))
