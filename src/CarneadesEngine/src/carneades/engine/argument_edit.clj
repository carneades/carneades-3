;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.engine.argument-edit
  (:use carneades.engine.argument
        carneades.engine.statement
        [clojure.contrib.core :only (dissoc-in)]))

;;; all functions to modify an existing argument graph

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; private functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- update-premise [arg oldstmt newstmt]
  (let [oldpremise (first (filter #(= oldstmt (:atom %)) (:premises arg)))
        newpremise (assoc oldpremise :atom newstmt)
        premises (filter #(not= oldstmt (:atom %)) (:premises arg))
        premises (conj premises newpremise)]
    (assoc arg :premises premises)))

(defn- update-premises [ag argids oldstmt newstmt]
  (reduce (fn [ag argid]
            (let [arg (get-argument ag argid)
                  newarg (update-premise arg oldstmt newstmt)]
              (assoc-in ag [:arguments argid] newarg)))
          ag argids))

(defn- update-conclusion [arg new]
  (assoc arg :conclusion new))

(defn- update-conclusions [ag argids new]
  (reduce (fn [ag argid]
            (let [oldarg (get-argument ag argid)
                  newarg (update-conclusion oldarg new)]
              (assoc-in ag [:arguments argid] newarg)))
          ag argids))

(defn- update-main-issue [ag oldstmt newstmt]
  (if (= (:main-issue ag) oldstmt)
    (assoc ag :main-issue newstmt)
    ag))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; public functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn update-statement-content [ag oldstmt newstmt]
  "returns the new ag or nil if oldsmt does not exist in ag"
  (when-let [n (statement-node ag oldstmt)]
    (let [key (statement-symbol (statement-atom oldstmt))
          ag (dissoc-in ag [:nodes key])
          n (assoc n :statement newstmt)
          ag (add-node ag n)
          ag (update-conclusions ag (:conclusion-of n) newstmt)
          ag (update-main-issue ag oldstmt newstmt)
          ag (update-premises ag (:premise-of n) oldstmt newstmt)]
      ag)))

(defn update-statement-proofstandard [ag stmt proofstandard]
  (when-let [n (statement-node ag stmt)]
   (let [n (assoc n :standard proofstandard)
         ag (add-node ag n)]
     (update-statement ag stmt))))

(defn update-premise-polarity [ag arg atom polarity]
  (letfn [(update-pm-polarity
           [arg]
           (let [pms (:premises arg)
                 pms (group-by (fn [pm]
                                 (= (:atom pm) atom)) pms)
                 toupdate (first (get pms true))
                 tokeep (get pms false)
                 updated (assoc toupdate :polarity polarity)]
             (assoc arg :premises (conj tokeep updated))))]
    (let [ag (update-in ag [:arguments (:id arg)] update-pm-polarity)
          newarg (get-argument ag (:id arg))]
      (update-argument ag newarg))))

(defn update-premise-type [ag arg atom type]
  (letfn [(update-pm-type
           [arg]
           (let [pms (:premises arg)
                 pms (group-by (fn [pm]
                                 (= (:atom pm) atom)) pms)
                 toupdate (first (get pms true))
                 tokeep (get pms false)
                 updated (assoc toupdate :type type)]
             (assoc arg :premises (conj tokeep updated))))]
    (let [ag (update-in ag [:arguments (:id arg)] update-pm-type)
          newarg (get-argument ag (:id arg))]
      (update-argument ag newarg))))

(defn update-argument-title [ag arg title]
  (letfn [(update-arg-title
           [arg]
           (assoc arg :title title))]
    (update-in ag [:arguments (:id arg)] update-arg-title)))

(defn update-argument-weight [ag arg weight]
  (letfn [(update-arg-weight
           [arg]
           (assoc arg :weight weight))]
    (let [ag (update-in ag [:arguments (:id arg)] update-arg-weight)
          arg (get-argument ag (:id arg))]
      (update-statement ag (:conclusion arg)))))
