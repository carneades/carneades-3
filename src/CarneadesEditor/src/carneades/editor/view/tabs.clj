;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.editor.view.tabs
  (:use clojure.contrib.def
        clojure.contrib.swing-utils
        carneades.editor.view.menu.mainmenu)
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
           (javax.swing.event ChangeListener)
           (carneades.editor.uicomponents EditorApplicationView)))

(defvar- *viewinstance* (EditorApplicationView/instance))

(defvar *mapPanel* (.mapPanel *viewinstance*))

(defvar- *closebutton-url* "carneades/editor/view/close-button.png")
(defvar- *closebutton-rollover-url*
  "carneades/editor/view/close-button-rollover.png")

(defvar- *close-button-listeners* (atom ()))

(defvar- *swingcomponents-to-ags* (ref {}) "components -> [path graphid]")
(defvar- *ags-to-components* (ref {}) "[path id] -> component")
(defvar- *undoable-components* (ref #{}))
(defvar- *redoable-components* (ref #{}))
(defvar- *component-to-title* (ref {}))
(defvar- *dirty-components* (atom #{}))

(defn get-graphinfo [component]
  (get (deref *swingcomponents-to-ags*) component))

(defn update-undo-redo-button-states [path id]
  (let [undoables (deref *undoable-components*)
        redoables (deref *redoable-components*)] 
    (if (contains? undoables [path id])
      (enable-undo-button)
      (disable-undo-button))
    (if (contains? redoables [path id])
         (enable-redo-button)
         (disable-redo-button))))

(defvar- *change-listener*
  (proxy [ChangeListener] []
    (stateChanged
     [evt]
     (let [idx (.getSelectedIndex *mapPanel*)]
       (if (= idx -1)
         (do
           (disable-save-button)
           (disable-undo-button)
           (disable-redo-button))
         (let [component (.getComponentAt *mapPanel* idx)
               [path id] (get-graphinfo component)]
           (if (contains? (deref *dirty-components*) component)
             (enable-save-button)
             (disable-save-button))
           (update-undo-redo-button-states path id)))))))

(defn init-tabs []
  (.setTabLayoutPolicy *mapPanel* JTabbedPane/SCROLL_TAB_LAYOUT)
  (.addChangeListener *mapPanel* *change-listener*))

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
    (.setBorder tabcomponent (BorderFactory/createEmptyBorder 0 0 0 0))
    (.setLayout tabcomponent (FlowLayout. FlowLayout/LEFT 0 0))
    (.add tabcomponent label)
    (.add tabcomponent closebutton)
    {:tabcomponent tabcomponent :label label}))

(defn get-tabtitle [ag isdirty]
  (if (empty? (:title ag))
    (str (if isdirty "*") (format "%s [title missing]" (:id ag)))
    (str (if isdirty "*") (:title ag))))

(defn add-component [graphcomponent path ag]
  (let [title (get-tabtitle ag false)
        id (:id ag)
        component (:component graphcomponent)
        tab (create-tabcomponent title)]
    (.add *mapPanel* title component)
    (.setTabComponentAt *mapPanel*
                        (.indexOfComponent *mapPanel* component)
                        (:tabcomponent tab))
    (.setSelectedComponent *mapPanel* component)
    (dosync
     (alter *swingcomponents-to-ags* assoc component [path id])
     (alter *ags-to-components* assoc [path id] graphcomponent)
     (alter *component-to-title* assoc component (:label tab)))))

(defn get-component [path id]
  (get (deref *ags-to-components*) [path id]))

(defn remove-component [graphcomponent]
  (let [component (:component graphcomponent)
        info (get-graphinfo component)]
    (.remove *mapPanel* component)
    (dosync
     (alter *swingcomponents-to-ags* dissoc component)
     (alter *component-to-title* dissoc component)
     (alter *ags-to-components* dissoc info)
     (alter *undoable-components* disj info)
     (alter *redoable-components* disj info))))

(defn tabs-empty? []
  "true if no tabs"
  (empty? (deref *swingcomponents-to-ags*)))

(defn select-component [component]
  (.setSelectedIndex *mapPanel*
                     (.indexOfComponent *mapPanel* (:component component))))

(defn set-component-can-undo [path id state]
  (dosync
   (if state
     (alter *undoable-components* conj [path id])
     (alter *undoable-components* disj [path id]))))

(defn set-component-can-redo [path id state]
  (dosync
   (if state
     (alter *redoable-components* conj [path id])
     (alter *redoable-components* disj [path id])))
  (update-undo-redo-button-states path id))

(defn set-component-dirty [graphcomponent ag isdirty]
  (let [component (:component graphcomponent)
        label (get (deref *component-to-title*) component)]
    (.setText label (get-tabtitle ag isdirty))
    (if isdirty
      (swap! *dirty-components* conj component)
      (swap! *dirty-components* disj component))))