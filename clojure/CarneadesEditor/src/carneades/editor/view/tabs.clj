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

(defvar- *tabPopupMenu* (.tabPopupMenu *viewinstance*))

(defn- show-popupmenu [event]
  (when (.isPopupTrigger event)
    (.show *tabPopupMenu*
           (.getComponent event)
           (.getX event)
           (.getY event))))

(defn- dispatch-panel-event [tabpanel event]
  (.dispatchEvent *mapPanel*
                  (SwingUtilities/convertMouseEvent
                   tabpanel event *mapPanel*)))

;; (deftype TabMouseMenuListener [] MouseListener
;;   (mouseClicked
;;    [this event]
;;    (dispatch-panel-event (.getSource event) event))
  
;;   (mouseEntered
;;    [this event]
;;    (dispatch-panel-event (.getSource event) event))
  
;;   (mouseExited
;;    [this event]
;;    (dispatch-panel-event (.getSource event) event))
  
;;   (mousePressed
;;    [this event]
;;    (dispatch-panel-event (.getSource event) event))
  
;;   (mouseReleased [this event]
;;    (dispatch-panel-event (.getSource event) event)))

;; (defvar- *tabMouseListener* (TabMouseMenuListener.))

(defvar- *closebutton-url* "carneades/editor/view/close-button.png")
(defvar- *closebutton-rollover-url*
  "carneades/editor/view/close-button-rollover.png")

(defvar- *close-button-listeners* (atom ()))

(defn register-close-button-listener [listener]
  (swap! *close-button-listeners* conj listener))

(defn create-close-button []
     (let [closebutton (JButton.)]
       (doto closebutton
         (.setBorder nil)
         (.setIcon (ImageIcon. (ClassLoader/getSystemResource
                                *closebutton-url*)))
         (.setRolloverIcon (ImageIcon. (ClassLoader/getSystemResource
                     *closebutton-rollover-url*)))
         ;; (.setFocusable false)
         (.setRolloverEnabled true))
       (doseq [listener (deref *close-button-listeners*)]
         (add-action-listener closebutton listener))
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

(defn get-tab [tabpanel title]
  "returns the index of the tab titled title or nil if it does not exist"
  (let [nbtabs (.getTabCount tabpanel)]
    (loop [n (dec nbtabs)]
      (if (neg? n)
        nil
        (if (= (.getTitleAt tabpanel n) title)
          n
          (recur (dec n)))))))

(defvar- *components-to-ags* (ref {}) "components -> [path graphid]")
(defvar- *ags-to-components* (ref {}) "[path id] -> component")

(defn add-component [component path id]
  (dosync
   (alter *components-to-ags* assoc component [path id])
   (alter *ags-to-components* assoc [path id] component)))

(defn get-graphinfo [component]
  (get (deref *components-to-ags*) component))

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
