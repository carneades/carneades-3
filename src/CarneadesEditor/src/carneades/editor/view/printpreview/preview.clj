(ns carneades.editor.view.printpreview.preview
  (:use clojure.contrib.def)
  (:import (javax.swing JPanel JFrame JButton)
           (java.awt Dimension Image Color image.BufferedImage)
           (java.awt.print PrinterJob Printable)
           (carneades.editor.uicomponents PrintPreviewView
                                          PreviewContainer
                                          PagePreview)))

(defvar- *horizontal-gap* 16)
(defvar- *vertical-gap* 10)

;; (defvar- *previewcontainer*
;;   (proxy [JPanel] []
;;     (getPreferredSize
;;      []
;;      (let [n (proxy-super getComponentCount)]
;;        (if (= n 0)
;;          (Dimension. *horizontal-gap* *vertical-gap*)
;;          (let [firstcomp (proxy-super getComponent 0)
;;                dim (.getPreferredSize firstcomp)
;;                width (.width dim)
;;                height (.height dim)
;;                parent (proxy-super getParent)
;;                parentdim (.getSize parent)
;;                ncol (max (/ (- (.width parentdim) *horizontal-gap*)
;;                             (+ width *horizontal-gap*))
;;                          1)
;;                nrow (int (/ n ncol))
;;                nrow (if (< (* nrow ncol) n) (inc nrow) nrow)
;;                windowwidth (+ (* ncol (+ height *horizontal-gap*))
;;                               *vertical-gap*)
;;                windowheight (+ (* nrow (+ height *vertical-gap*))
;;                                *vertical-gap*)
;;                insets (proxy-super getInsets)]
;;            (let [w (+ windowwidth (.left insets) (.right insets))
;;                  h (+ windowheight (.top insets) (.bottom insets))]
;;              (prn "previewcontainer")
;;              (prn "w")
;;              (prn w)
;;              (prn "h")
;;              (prn h)
;;              (Dimension. w h))))))
;;     (getMaximumSize
;;      []
;;      (.getPreferredSize this))
;;     (getMinimumSize
;;      []
;;      (.getPreferredSize this))
;;     (doLayout
;;      []
;;      (let [insets (proxy-super getInsets)
;;            x (+ (.left insets) *horizontal-gap*)
;;            y (+ (.top insets) *vertical-gap*)
;;            n (proxy-super getComponentCount)]
;;        (when-not (= n 0)
;;          (let [firstcomp (proxy-super getComponent 0)
;;                dim (.getPreferredSize firstcomp)
;;                width (.width dim)
;;                height (.height dim)
;;                parentdim (.getSize (proxy-super getParent))
;;                ncol (max (/ (- (.width parentdim) *horizontal-gap*)
;;                             (+ width *horizontal-gap*))
;;                          1)
;;                nrow (int (/ n ncol))
;;                nrow (if (< (* nrow ncol) n) (inc nrow) nrow)]
;;            (letfn [(place-component
;;                     [index]
;;                     (loop [index index
;;                            m 0
;;                            x x]
;;                       (cond (>= index n) nil
;;                             (>= m ncol) [x index]
;;                             :else
;;                             (let [compo (proxy-super getComponent index)]
;;                               (.setBounds compo x y width height)
;;                               (recur (inc index) (inc m)
;;                                      (+ x width *horizontal-gap*)))
;;                             )))]
;;              (loop [k 0
;;                     y y
;;                     index 0]
;;              (when (< k nrow)
;;                (when-let [[x index]  (place-component index)]
;;                  (recur (inc k) (+ y (.left insets) *horizontal-gap*)
;;                         index)))))))))))

;; (defn get-pagepreview [width height source]
;;   (let [w (atom width)
;;         h (atom height)
;;         img (atom (.getScaledInstance source width height Image/SCALE_SMOOTH))
;;         proxy (proxy [JPanel] []
;;                 (setScaledSize
;;                  [wi he]
;;                  (reset! w wi)
;;                  (reset! h he)
;;                  (reset! img (.getScaledInstance (deref img)
;;                                                  wi
;;                                                  he
;;                                                  Image/SCALE_SMOOTH))
;;                  (proxy-super repaint))
                
;;                 (paint
;;                  [g]
;;                  (.setColor g (proxy-super getBackground))
;;                  (.fillRect g 0 0 (proxy-super getWidth) (proxy-super getHeight))
;;                  (.drawImage g (deref img) 0 0 this)
;;                  (proxy-super paintBorder g)))]
;;     (.flush (deref img))
;;     proxy
;;     ))

(defn printpreview [graphcomponent]
  (let [previewframe (PrintPreviewView.)
        mainpanel (.mainPanel previewframe)
        printjob (PrinterJob/getPrinterJob)
        pageformat (.defaultPage printjob)
        wpage (int (.getWidth pageformat))
        hpage (int (.getHeight pageformat))
        scale 10
        w (int (/ (* wpage scale) 100))
        h (int (/ (* hpage scale) 100))
        previewcontainer (PreviewContainer.)]
    (loop [img (BufferedImage. wpage hpage BufferedImage/TYPE_INT_RGB)
           pageindex 0]
      (let [g (.getGraphics img)]
        (.setColor g Color/white)
        (.fillRect g 0 0 wpage hpage)
        (when (= (.print graphcomponent g pageformat pageindex)
                 Printable/PAGE_EXISTS)
          (.add previewcontainer (PagePreview. wpage hpage img)
                ;; (get-pagepreview wpage hpage img)
                )
          (recur (BufferedImage. wpage hpage BufferedImage/TYPE_INT_RGB)
                 (inc pageindex)))))
    (.add mainpanel previewcontainer)
    (.revalidate mainpanel)
    (.setDefaultCloseOperation previewframe JFrame/DISPOSE_ON_CLOSE)
    (.setVisible previewframe true)))

