;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.editor.view.printing.preview
  (:use clojure.contrib.def
        clojure.contrib.swing-utils
        carneades.mapcomponent.map
        [clojure.string :only (split)])
  (:import (javax.swing JPanel JFrame JButton)
           (javax.print.attribute HashPrintRequestAttributeSet
                                  standard.OrientationRequested)
           (java.awt Dimension Image Color image.BufferedImage)
           (java.awt.print PrinterJob Printable PageFormat Book)
           (carneades.editor.uicomponents PrintPreviewDialog
                                          PreviewContainer
                                          PagePreview)))

;; we don't follow the MVC pattern for the print preview frame
;; the view register the listeners itself
;; this allow to build an autonome component for any print task

(defvar- *horizontal-gap* 16)
(defvar- *vertical-gap* 10)
(defvar- *scale* (atom 100))

(defn print-document
  ([printable]
     (let [printjob (PrinterJob/getPrinterJob)
           pageformat (.defaultPage printjob)]
       (.setOrientation pageformat PageFormat/LANDSCAPE)
       (print-document printable pageformat)))
  ([printable pageformat]
     (let [printjob (PrinterJob/getPrinterJob)
           book (Book.)
           attributes (HashPrintRequestAttributeSet.)]
       (cond (= (.getOrientation pageformat) PageFormat/LANDSCAPE)
             (.add attributes OrientationRequested/LANDSCAPE)
             
             (= (.getOrientation pageformat) PageFormat/PORTRAIT)
             (.add attributes OrientationRequested/PORTRAIT))
       ;; (.append book printable pageformat)
       ;; (.setPageable printjob book)
       (when (.printDialog printjob attributes)
         (.setPrintable printjob printable)
         (.print printjob attributes)))))

(defn- fill-previewcontainer [previewcontainer printable pageformat]
  (let [wpage (int (.getWidth pageformat))
        hpage (int (.getHeight pageformat))
        scale 100;; (deref *scale*)
        w (int (/ (* wpage scale) 100))
        h (int (/ (* hpage scale) 100))]
    (.removeAll previewcontainer)
    (loop [img (BufferedImage. wpage hpage BufferedImage/TYPE_INT_RGB)
           pageindex 0]
      (let [g (.getGraphics img)]
        (.setColor g Color/white)
        (.fillRect g 0 0 wpage hpage)
        (when (= (.print printable g pageformat pageindex)
                 Printable/PAGE_EXISTS)
          (.add previewcontainer (PagePreview. w h img))
          (recur (BufferedImage. wpage hpage BufferedImage/TYPE_INT_RGB)
                 (inc pageindex)))))
    (.revalidate previewcontainer)
    (.repaint previewcontainer)))

(defn- print-button-listener [event printable pageformat]
  (print-document printable pageformat))

(defn- portrait-button-listener [event landscapebutton
                                 previewcontainer printable pageformat]
  (let [portrait (.getSource event)]
    (when (.isSelected portrait)
      (.setSelected landscapebutton false)
      (.setOrientation pageformat PageFormat/PORTRAIT)
      (fill-previewcontainer previewcontainer printable pageformat))))

(defn- landscape-button-listener [event portraitbutton previewframe
                                  printable pageformat]
  (let [landscape (.getSource event)]
    (when (.isSelected landscape)
      (.setSelected portraitbutton false)
      (.setOrientation pageformat PageFormat/LANDSCAPE)
      (fill-previewcontainer previewframe printable pageformat))))

(defn- scale-combobox-listener [event previewcontainer printable pageformat]
  (letfn [(parse-value
           [s]
           (Integer/parseInt (first (split s #"%"))))]
    (let [combobox (.getSource event)
          scale (parse-value (.getSelectedItem combobox))]
      (reset! *scale* scale)
      (let [scaleval (/ 100 (double scale))]
        (scale-page printable scaleval))
      (fill-previewcontainer previewcontainer printable pageformat))))

(defn- attach-listeners [previewframe previewcontainer printable pageformat]
  (let [printbutton (.printButton previewframe)
        closepreviewbutton (.closePreviewButton previewframe)
        landscapetogglebutton (.landscapeToggleButton previewframe)
        portraittogglebutton (.portraitToggleButton previewframe)
        scalecombobox (.scaleComboBox previewframe)]
    (add-action-listener printbutton print-button-listener printable pageformat)
    (add-action-listener closepreviewbutton (fn [event] (.dispose previewframe)))
    (add-action-listener portraittogglebutton portrait-button-listener
                         landscapetogglebutton previewcontainer printable
                         pageformat)
    (add-action-listener landscapetogglebutton landscape-button-listener
                         portraittogglebutton previewcontainer printable pageformat)
    (add-action-listener scalecombobox scale-combobox-listener previewcontainer
                         printable pageformat)))

(defn printpreview [parent printable]
  (let [previewframe (PrintPreviewDialog. parent true)
        mainpanel (.mainPanel previewframe)
        printjob (PrinterJob/getPrinterJob)
        pageformat (.defaultPage printjob)
        previewcontainer (PreviewContainer.)
        landscapetogglebutton (.landscapeToggleButton previewframe)]
    (reset! *scale* 100)
    (scale-page printable 1)
    (.setSelected landscapetogglebutton true)
    (.setOrientation pageformat PageFormat/LANDSCAPE)
    (attach-listeners previewframe previewcontainer printable pageformat)
    (fill-previewcontainer previewcontainer printable pageformat)
    (.add mainpanel previewcontainer)
    (.revalidate mainpanel)
    (.setDefaultCloseOperation previewframe JFrame/DISPOSE_ON_CLOSE)
    (.setVisible previewframe true)))

