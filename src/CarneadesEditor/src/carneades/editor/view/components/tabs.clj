;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.editor.view.components.tabs
  (:use clojure.contrib.def
        clojure.contrib.trace
        clojure.contrib.swing-utils
        carneades.editor.utils.listeners
        carneades.editor.view.components.uicomponents)
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


(defvar *mapPanel* (.mapPanel *frame*))

;; (defvar- *closebutton-url* "carneades/editor/view/close-button.png")
(defvar- *closebutton-url* "close-button.png")
;; (defvar- *closebutton-rollover-url*
;;   "carneades/editor/view/close-button-rollover.png")
(defvar- *closebutton-rollover-url*
  "close-button-rollover.png")

(gen-listeners-fns "close-button")

(defvar- *swingcomponents-to-ags* (ref {}) "components -> [path graphid]")
(defvar- *ags-to-components* (ref {}) "[path id] -> component")
(defvar- *component-to-title* (ref {}))

(defn get-graphinfo [component]
  (get (deref *swingcomponents-to-ags*) component))

(defn init-tabs []
  (.setTabLayoutPolicy *mapPanel* JTabbedPane/SCROLL_TAB_LAYOUT))

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
    (prn "creating label, title =")
    (prn title)
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

(defn add-component [graphcomponent path ag isdirty]
  (let [title (get-tabtitle ag isdirty)
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
     (alter *ags-to-components* dissoc info))))

(defn tabs-empty? []
  "true if no tabs"
  (empty? (deref *swingcomponents-to-ags*)))

(defn select-component [component]
  (.setSelectedIndex *mapPanel*
                     (.indexOfComponent *mapPanel* (:component component))))

(defn set-tab-dirty [path id isdirty]
  (if-let [component (:component (get (deref *ags-to-components*) [path id]))]
    (if-let [label (get (deref *component-to-title*) component)]
      (let [oldtext (.getText label)
            olddirty (= (first oldtext) \*)]
        (when-not (= olddirty isdirty)
          (if isdirty
            (.setText label (str "*" oldtext))
            (.setText label (.substring oldtext 1))))))))

(defn register-tab-change-listener [listener]
  (.addChangeListener *mapPanel* listener))

(defn change-tab-title [component newtitle]
  "change the title of a clean component"
  (prn "change-tab-title")
  (when-let [label (get (deref *component-to-title*) (:component component))]
    (.setText label newtitle)))