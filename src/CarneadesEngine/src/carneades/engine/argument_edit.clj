;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.engine.argument-edit
  (:use clojure.contrib.pprint
        carneades.engine.argument
        carneades.engine.statement
        [clojure.contrib.core :only (dissoc-in)]))

;;; all functions to modify an existing argument graph

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; private functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
    (let [key (statement-symbol oldstmt)
          ag (update-in ag [:nodes] dissoc key)
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

(defn update-premise-role [ag arg atom role]
  (letfn [(update-pm-role
           [arg]
           (let [pms (:premises arg)
                 pms (group-by (fn [pm]
                                 (= (:atom pm) atom)) pms)
                 toupdate (first (get pms true))
                 tokeep (get pms false)
                 updated (assoc toupdate :role role)]
             (assoc arg :premises (conj tokeep updated))))]
    (update-in ag [:arguments (:id arg)] update-pm-role)))

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

(defn update-argument-direction [ag arg direction]
  (letfn [(update-arg-direction
           [arg]
           (assoc arg :direction direction))]
    (let [ag (update-in ag [:arguments (:id arg)] update-arg-direction)
          arg (get-argument ag (:id arg))]
      (update-statement ag (:conclusion arg)))))

(defn add-premise [ag arg stmt]
  "add a premise and returns the new argument graph or nil
   if the premise would introduce a cycle"
  (letfn [(add-premise-to-arg
           [arg]
           (update-in arg [:premises] conj (pm stmt)))]
    (let [newarg (update-in arg [:premises] conj (pm stmt))]
      (when (cycle-free? ag newarg)
        (let [ag (update-in ag [:arguments (:id arg)] (fn [_] newarg))
              key (statement-symbol stmt)
              ag (update-in ag [:nodes key (statement-atom stmt) :premise-of] conj (:id arg))
              newarg (get-argument ag (:id arg))]
          (update-argument ag newarg))))))

(defn delete-premise [ag arg pm]
  (letfn [(delete-premise-from-arg
           [arg]
           (assoc arg :premises (filter #(not= pm %) (:premises arg))))]
   (let [ag (update-in ag [:arguments (:id arg)] delete-premise-from-arg)
         ag (update-in ag [:nodes (statement-symbol (:atom pm))
                           (statement-atom (:atom pm)) :premise-of] disj (:id arg))
         newarg (get-argument ag (:id arg))]
     (update-argument ag newarg))))

(defn delete-argument [ag arg]
  (let [conclusion (:conclusion arg)
        key (statement-symbol conclusion)
        ag (update-in ag [:nodes key conclusion :conclusion-of] disj (:id arg))
        ag (update-in ag [:arguments] dissoc (:id arg))
        ag (reduce (fn [ag pm]
                        (let [key (statement-symbol (:atom pm))
                              stmt (statement-atom (:atom pm))]
                          (update-in ag [:nodes key stmt :premise-of] disj (:id arg))))
                      ag
                      (:premises arg))
        ag (reduce (fn [ag pm]
                     (update-statement ag (:atom pm)))
                   ag
                   (:premises arg))]
    (update-statement ag (:conclusion arg))))

(defn delete-statement [ag stmt]
  (when-let [node (statement-node ag stmt)]
    (let [premises-of (:premise-of node)
          conclusion-of (:conclusion-of node)
          key (statement-symbol stmt)
          main-issue (:main-issue ag)
          ag (reduce (fn [ag argid]
                       (let [arg (get-argument ag argid)
                             pm (get-premise arg stmt)]
                        (delete-premise ag arg pm)))
                     ag premises-of)
          ag (reduce (fn [ag argid]
                       (delete-argument ag (get-argument ag argid)))
                     ag conclusion-of)
          ag (update-in ag [:nodes] dissoc key)]
      (if (= main-issue stmt)
        (assoc ag :main-issue nil)
        ag))))

(defn change-mainissue [ag stmt]
 (assoc ag :main-issue stmt))
