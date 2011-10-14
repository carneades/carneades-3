;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1


(ns ^{:doc "Implementations of the various proof standards."}
    carneades.engine.proofstandard
  (:use clojure.contrib.def))

;; dispatch on the proof standard
(defmulti satisfies-proof-standard? (fn [_ ps _ _ _] ps))

;; scintilla? (deprecated) 
(defmethod satisfies-proof-standard? :se [ag ps pro-args con-args all-premises-hold?]
  "argument-graph (seq-of argument) (seq-of argument) -> boolean"
  (not (nil? (some #(all-premises-hold? ag %) pro-args))))

;; dialectically-valid?
(defmethod satisfies-proof-standard? :dv [ag ps pro-args con-args all-premises-hold?]
  "argument-graph (seq-of argument) (seq-of argument) -> boolean"
  (and (satisfies-proof-standard? ag :se pro-args con-args all-premises-hold?)
       (not-any? #(all-premises-hold? ag %) con-args)))

(defn- best-arg [ags]
  (if (empty? ags) 0.0 (apply max (map :weight ags))))

(defn- preponderance [ag ps pro-args con-args all-premises-hold?]
  (let [pro (filter #(all-premises-hold? ag %) pro-args)
        con (filter #(all-premises-hold? ag %) con-args)
        best-pro (best-arg pro)
        best-con (best-arg con)]
    (> best-pro best-con)))

;;; deprecated!
(defmethod satisfies-proof-standard? :ba [ag ps pro-args con-args all-premises-hold?]
  "argument-graph (seq-of argument) (seq-of argument) -> boolean"
  (preponderance ag ps pro-args con-args all-premises-hold?))

(defmethod satisfies-proof-standard? :pe [ag ps pro-args con-args all-premises-hold?]
  "argument-graph (seq-of argument) (seq-of argument) -> boolean"
  ;; preponderance of the evidence
  (preponderance ag ps pro-args con-args all-premises-hold?))

;; clear-and-convincing-evidence?
(defmethod satisfies-proof-standard? :cce [ag ps pro-args con-args all-premises-hold?]
  "argument-graph (seq-of argument) (seq-of argument) -> boolean"
  (let [pro (filter #(all-premises-hold? ag %) pro-args)
        con (filter #(all-premises-hold? ag %) con-args)
        best-pro (best-arg pro)
        best-con (best-arg con)
        alpha 0.5
        beta 0.3]
    (and (> best-pro best-con) ; i.e. preponderance of the evidence test is met
         (> best-pro alpha)
         (> (- best-pro best-con) beta))))

;; beyond-reasonable-doubt?
(defmethod satisfies-proof-standard? :brd [ag ps pro-args con-args all-premises-hold?]
  "argument-graph (list-of argument) (list-of argument) -> boolean"
  (let [pro (filter #(all-premises-hold? ag %) pro-args)
        con (filter #(all-premises-hold? ag %) con-args)
        best-pro (best-arg pro)
        best-con (best-arg con)
        alpha 0.5
        beta 0.5
        gamma 0.2]
    (and
     ; clear and convincing evidence test is also met
     (> best-pro best-con)
     (> best-pro alpha)
     (> (- best-pro best-con) beta)
     (< best-con gamma))))
