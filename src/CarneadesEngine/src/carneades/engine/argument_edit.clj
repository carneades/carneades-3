;;; Copyright (c) 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns ^{:doc "Functions to modify an argument graph in a consistent way.
            
            WARNING: This code is out of date and needs to be ported to
            use the new argument-graph module.  It probably should be
            integrated into argument-graph module instead of remaining
            a separate module."}
    carneades.engine.argument-edit
  (:use clojure.pprint
        carneades.engine.argument
        carneades.engine.statement
        ;; [clojure.contrib.core :only (dissoc-in)]
        ))

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

(defn update-statement-content
  "Returns the new ag or nil if oldsmt does not exist in ag"
  [ag oldstmt newstmt]
  (when-let [n (statement-node ag oldstmt)]
    (let [key (statement-symbol oldstmt)
          ag (update-in ag [:nodes key] dissoc oldstmt)
          n (assoc n :statement newstmt)
          ag (add-node ag n)
          ag (update-conclusions ag (:conclusion-of n) newstmt)
          ag (update-main-issue ag oldstmt newstmt)
          ag (update-premises ag (:premise-of n) oldstmt newstmt)]
      ag)))

(defn update-statement-proofstandard
  "Updates the proofstandard of a statement (conclusion)"
  [ag stmt proofstandard]
  (when-let [n (statement-node ag stmt)]
   (let [n (assoc n :standard proofstandard)
         ag (add-node ag n)]
     (update-statement ag stmt))))

(defn update-premise-polarity
  "Updates a premise polarity. Atom is a premise of arg."
  [ag arg atom polarity]
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

(defn update-premise-type
  "Updates a premise type. Atom is a premise of arg."
  [ag arg atom type]
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

(defn update-premise-role
  "Updates a premise role. Atom is a premise of arg."
  [ag arg atom role]
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

(defn- update-argument-val [ag arg k v]
  (letfn [(update-arg-val
           [arg]
           (assoc arg k v))]
    (update-in ag [:arguments (:id arg)] update-arg-val)))

(defn update-argument-title
  "Updates an argument title"
  [ag arg title]
  (update-argument-val ag arg :title title))

(defn update-argument-scheme
  "Updates an argument scheme"
  [ag arg scheme]
  (update-argument-val ag arg :scheme scheme))

(defn update-argument-weight
  "Updates an argument weight"
 [ag arg weight]
  (let [ag  (update-argument-val ag arg :weight weight) 
        arg (get-argument ag (:id arg))]
    (update-statement ag (:conclusion arg))))

(defn update-argument-direction
  "Updates an argument direction"
  [ag arg direction]
  (let [ag (update-argument-val ag arg :direction direction)
        arg (get-argument ag (:id arg))]
    (update-statement ag (:conclusion arg))))

(defn add-premise
  "Add a premise and returns the new argument graph or nil
   if the premise would introduce a cycle"
  [ag arg stmt]
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

(defn delete-premise
  "Deletes a premise"
  [ag arg pm]
  (letfn [(delete-premise-from-arg
           [arg]
           (assoc arg :premises (filter #(not= pm %) (:premises arg))))]
   (let [ag (update-in ag [:arguments (:id arg)] delete-premise-from-arg)
         ag (update-in ag [:nodes (statement-symbol (:atom pm))
                           (statement-atom (:atom pm)) :premise-of] disj (:id arg))
         newarg (get-argument ag (:id arg))]
     (update-argument ag newarg))))

(defn delete-argument
  "Deletes an argument"
  [ag arg]
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

(defn delete-statement
  "Deletes a statement"
  [ag stmt]
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
          ag (update-in ag [:nodes key] dissoc stmt)]
      (if (= main-issue stmt)
        (assoc ag :main-issue nil)
        ag))))

(defn change-mainissue
  "Changes the main-issue"
  [ag stmt]
  (assoc ag :main-issue stmt))
