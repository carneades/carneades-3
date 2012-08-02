;;; Copyright (c) 2010 Fraunhofer Gesellschaft
;;; Licensed under the EUPL V.1.1

(ns ^{:doc "Abductive reasoning with argument graphs.
            
            WARNING: This code is out of date and needs to be ported to use the 
            new argument-graph module.

            See:
            Ballnat, S. and Gordon, T.F. Goal Selection in Argumentation Processes 
            - A Formal Model of Abduction in Argument Evaluation Structures. Computational Models of Argument 
            - Proceedings of COMMA 2010, IOS Press (2010), 51-62."}
  carneades.engine.abduction
  (:use clojure.pprint
        clojure.set
        carneades.engine.utils
        carneades.engine.argument
        carneades.engine.statement))

;; (defn format-label
;;   [l]
;;   (map (fn [p] (map (fn [s] (literal->str s)) p)) l))

;; (def *verum-clause* #{true})
;; (def *verum* #{*verum-clause*})
;; (def *falsum-clause* #{false})
;; (def *falsum* #{*falsum-clause*})

;; (declare argument-in-label
;;   argument-out-label
;;   conjunct-dnf-with-dnf conjunct-clause-with-dnf
;;   combine-conjunction-of-dnf
;;   true-filter
;;   collect-labels-conj)

;; ;; dispatch on the proof standard
;; (defmulti ps-in-label (fn [ag asm s] (proof-standard ag s)))

;; (defmulti ps-out-label (fn [ag asm s] (proof-standard ag s)))

;; (defmethod ps-in-label :default [ag asm s]
;;   #{#{s}})

;; (defmethod ps-out-label :default [ag asm s]
;;   #{#{(statement-complement s)}})

;; (defn statement-in-label [ag asm s]
;;   {:pre [(set? asm)]}
;;   "argument-graph (set-of statement) statement -> dnf"
;;   (cond (contains? asm s) *verum*
;;     (contains? asm (statement-complement s)) #{#{s}}
;;     :else (ps-in-label ag asm s)))

;; (defn statement-out-label [ag asm s]
;;   {:pre [(set? asm)]}
;;   (cond (contains? asm s) #{#{(statement-complement s)}}
;;     (contains? asm (statement-complement s)) *verum*
;;     :else (ps-out-label ag asm s)))

;; (defn argument-in-label [ag asm arg]
;;   {:pre [(set? asm)]}
;;   (letfn [(collect-labels
;;             [f statements]
;;             (collect-labels-conj #(f ag asm (premise-statement %)) statements))]
;;     (let [pr (argument-premises arg)
;;           [pr-labels pr-labels-true] (collect-labels statement-in-label pr)
;;           l (set (map true-filter (combine-conjunction-of-dnf pr-labels)))]
;; ;      (println "------------")
;; ;      (println "out-goals for       :" (str (:scheme arg) "-" (:id arg)))
;; ;      (println "pr-labels           :" (map format-label pr-labels))
;; ;      (println "pr-labels-true      :" pr-labels-true)
;; ;      (println "combination         :" (combine-conjunction-of-dnf pr-labels)
;; ;      (println "label               :" (format-label l))
;; ;      (println "------------")
;;       l)))

;; (defn collect-labels-conj
;;   "returns [labels alltrue]
;; alltrue is true if coll is empty or if each value is equal to *verum*"
;;   [get-label coll]
;;   {:pre [(not (nil? coll))]}
;;   (reduce (fn [[labels alltrue] p]
;;             (let [label (get-label p)]
;;               (if (or (not= label *verum*) (not alltrue))
;;                 [(conj labels label) false]
;;                 [(conj labels label) true])))
;;     [#{} true] coll))

;; (defn collect-labels-conj-break
;;   "returns [labels alltrue onefalse]
;; stop collecting when *falsum* is encountered
;; alltrue is true if coll is empty or if each value is equal to *verum*"
;;   [get-label coll]
;;   {:pre [(not (nil? coll))]}
;;   (loop [coll coll
;;          labels #{}
;;          alltrue true
;;          onefalse false]
;;     (if (empty? coll)
;;       [labels alltrue onefalse]
;;       (let [label (get-label (first coll))]
;;         (if (= label *falsum*)
;;           [(conj labels label) false true] ; changed all-true -> false
;;           (if (or (not= label *verum*) (not alltrue))
;;             (recur (set (next coll)) (conj labels label) false onefalse)
;;             (recur (set (next coll)) (conj labels label) alltrue onefalse)))))))

;; (defn collect-labels-disj
;;   "Stops collecting when a seq of label contains *verum-clause*
;;    returns [labels dis-is-true dis-is-false]"
;;   [get-label coll]
;;   {:pre [(not (nil? coll))]}
;;   (loop [coll coll
;;          labels ()]
;;     (if (empty? coll)
;;       (if (empty? labels)
;;         [labels false true]  ;; the disjunction is false
;;         [labels false false]) ;; we don't know yet
;;       (let [p (first coll)
;;             label (get-label p)]
;;         (if (.contains label *verum-clause*)
;;           [labels true false] ;; the disjunction is true
;;           (recur (set (next coll)) (union labels label)))))))

;; (defn argument-out-label [ag asm arg]
;;   {:pre [(set? asm)]}
;;   (letfn [(collect-labels
;;             [f statements]
;;             (collect-labels-disj #(f ag asm (premise-statement %)) statements))]
;;     (let [pr (argument-premises arg)
;;           [pr-labels pr-labels-true] (collect-labels statement-out-label pr)
;;           l (cond pr-labels-true *verum*
;;               (empty? pr-labels) *falsum*
;;               :else (set (map true-filter pr-labels)))]
;; ;      (println "------------")
;; ;      (println "out-goals for      :" (str (:scheme arg) "-" (:id arg)))
;; ;      (println "premises           :" (map literal->str (map :atom pr)))
;; ;      (println "pr-labels          :" pr-labels)
;; ;      (println "pr-labels-true     :" pr-labels-true)
;; ;      (println "label              :" (format-label l))
;; ;      (println "------------")
;;       l
;;       )))

;; (defn combine-conjunction-of-dnf [dnfs]
;;   (cond (empty? dnfs) ()
;;     (empty? (next dnfs)) (first dnfs)
;;     :else (conjunct-dnf-with-dnf (first dnfs)
;;             (combine-conjunction-of-dnf (set (next dnfs))))))

;; ;;  '((A B) (AA BB)) '((C D)) -> ((A B C D) (AA BB C D))
;; (defn conjunct-dnf-with-dnf [dnf dnf2]
;;   (set (mapcat (fn [c] (conjunct-clause-with-dnf c dnf2)) dnf)))

;; ;; '(A B) '( (X Y Z) (X2 Y2 Z2) ) -> ((A B X Y Z) (A B X2 Y2 Z2))
;; (defn conjunct-clause-with-dnf [clause dnf]
;;   (set (map #(union % clause) dnf)))

;; (defn true-filter [col]
;;   (set (remove true? col)))

;; (defmethod ps-in-label :se [ag asm s]
;;   {:pre [(set? asm)]}
;;   (letfn [(collect-arguments-labels-disj
;;             [f arguments]
;;             (collect-labels-disj
;;               #(f ag asm %) arguments))]
;;     (let [pro-args (pro-arguments ag s)
;;           [pro-labels pro-labels-true]
;;           (collect-arguments-labels-disj argument-in-label pro-args)          
;;           l (cond 
;;               pro-labels-true *verum*
;;               :else (conj pro-labels #{s}))]
;;       l
;;       )))

;; (defmethod ps-out-label :se [ag asm s]
;;   {:pre [(set? asm)]}
;;   (letfn [(collect-arguments-labels-conj
;;             [f arguments]
;;             (collect-labels-conj-break #(f ag asm %) arguments))]
;;     (let [pro-args (pro-arguments ag s)
;;           [pro-labels pro-labels-true pro-labels-onefalse]
;;           (collect-arguments-labels-conj argument-out-label pro-args)       
;;           l (cond 
;;               pro-labels-onefalse *falsum*                            
;;               pro-labels-true *verum*
;;               :else (set (map true-filter
;;                             (combine-conjunction-of-dnf
;;                               pro-labels))))]
;;       l
;;       )))

;; (defmethod ps-in-label :dv [ag asm s]
;;   {:pre [(set? asm)]}
;;   (letfn [(collect-arguments-labels-disj
;;             [f arguments]
;;             (collect-labels-disj
;;               #(f ag asm %) arguments))
;;           (collect-arguments-labels-conj
;;             [f arguments]
;;             (collect-labels-conj-break #(f ag asm %) arguments))]
;;     (let [pro-args (pro-arguments ag s)
;;           con-args (con-arguments ag s)
;;           [pro-labels pro-labels-true]
;;           (collect-arguments-labels-disj argument-in-label pro-args)
;;           [con-labels con-labels-true con-labels-onefalse]
;;           (collect-arguments-labels-conj argument-out-label con-args)
;;           l (cond con-labels-onefalse #{#{s}}
;;               (and pro-labels-true con-labels-true) *verum*
;;               pro-labels-true (conj (set (map true-filter
;;                                       (combine-conjunction-of-dnf con-labels)))
;;                                 #{s})
;;               con-labels-true (conj pro-labels #{s})
;;               :else (conj (set (map true-filter
;;                             (combine-conjunction-of-dnf
;;                               (conj con-labels pro-labels))))
;;                       #{s}))]
;;       l
;;       )))

;; (defmethod ps-out-label :dv [ag asm s]
;;   {:pre [(set? asm)]}
;;   (letfn [(collect-arguments-conj
;;             [f arguments]
;;             (collect-labels-conj-break #(f ag asm %) arguments))
;;           (collect-arguments-disj
;;             [f arguments]
;;             (collect-labels-disj #(f ag asm %) arguments))]
;;     (let [pro-args (pro-arguments ag s)
;;           con-args (con-arguments ag s)
;;           [pro-labels pro-labels-true pro-labels-onefalse]
;;           (collect-arguments-conj argument-out-label pro-args)
;;           [con-labels con-labels-true]
;;           (collect-arguments-disj argument-in-label con-args)
;;           l (cond (or pro-labels-true con-labels-true) *verum*
;;               pro-labels-onefalse (conj con-labels #{(statement-complement s)})
;;               :else (conj (set (map true-filter
;;                             (union con-labels
;;                               (combine-conjunction-of-dnf pro-labels))))
;;                       #{(statement-complement s)}))]
;;       l
;;       )))

;; (defmethod ps-in-label :pe [ag asm s]
;;   {:pre [(set? asm)]}
;;   ;(println "computing pe-in-label for" s)
;;   (let [pro-args (pro-arguments ag s)
;;         con-args (con-arguments ag s)]
;;     (loop [pro-args pro-args
;;            labels #{}]
;;       (if (empty? pro-args)
;;         (conj labels #{s})
;;         (let [pro-arg (first pro-args)
;;               w (sget pro-arg :weight)
;;               greater-cons (filter #(<= w (sget % :weight)) con-args)
;;               pro-label (argument-in-label ag asm pro-arg)
;;               [con-labels _ onefalse] (collect-labels-conj-break
;;                                         #(argument-out-label ag asm %)
;;                                         greater-cons)
;;               collectedlabels (if onefalse
;;                                 *falsum*
;;                                 (combine-conjunction-of-dnf
;;                                   (if (= pro-label *verum*)
;;                                     (remove #(= *verum* %) con-labels)
;;                                     (conj (remove #(= *verum* %) con-labels)
;;                                       pro-label
;;                                       )
;;                                     )))]
;;           (cond (empty? collectedlabels) *verum*
;;             (= collectedlabels *falsum*) (recur (set (next pro-args)) labels)
;;             :else (recur (set (next pro-args))
;;                     (union labels collectedlabels))))))))

;; (defmethod ps-out-label :pe [ag asm s]
;;   {:pre [(set? asm)]}
;;   ;(println "computing pe-out-label for" s)
;;   (let [pro-args (pro-arguments ag s)
;;         con-args (con-arguments ag s)
;;         l (loop [pro-args pro-args
;;                  labels #{}]
;;             (if (empty? pro-args)
;;               (if (empty? labels)
;;                 *verum*
;;                 (conj (combine-conjunction-of-dnf labels)
;;                     #{(statement-complement s)}))
;;               (let [pro-arg (first pro-args)
;;                     w (sget pro-arg :weight)
;;                     greater-cons (filter #(<= w (sget % :weight)) con-args)
;;                     pro-label (argument-out-label ag asm pro-arg)
;;                     [con-labels con-labels-true con-labels-false]
;;                     (collect-labels-disj #(argument-in-label ag asm %)
;;                       greater-cons)]
;;                 (cond
;;                   (or (= pro-label *verum*) con-labels-true) (recur (set (next pro-args)) labels),
;;                   (and (= pro-label *falsum*) con-labels-false) #{#{(statement-complement s)}},
;;                   (= pro-label *falsum*) (recur (set (next pro-args))
;;                                             (conj labels con-labels)),
;;                   con-labels-false (recur (set (next pro-args))
;;                                      (conj labels pro-label)),
;;                   :else (recur (set (next pro-args))
;;                           (conj labels
;;                             (union pro-label con-labels)))))))]
;;     l))

;; (defmethod ps-in-label :cce [ag asm s]
;;   {:pre [(set? asm)]}
;;   ;(println "computing pe-in-label for" s)
;;   (let [alpha 0.5
;;         beta 0.3
;;         pro-args (filter (fn [a] (> (sget a :weight) alpha)) (pro-arguments ag s))
;;         con-args (con-arguments ag s)]
;;     (loop [pro-args pro-args
;;            labels #{}]
;;       (if (empty? pro-args)
;;         (conj labels #{s})
;;         (let [pro-arg (first pro-args)
;;               w (sget pro-arg :weight)
;;               greater-cons (filter #(<= (- w (sget % :weight)) beta) con-args)
;;               pro-label (argument-in-label ag asm pro-arg)
;;               [con-labels _ onefalse] (collect-labels-conj-break
;;                                         #(argument-out-label ag asm %)
;;                                         greater-cons)
;;               collectedlabels (if onefalse
;;                                 *falsum*
;;                                 (combine-conjunction-of-dnf
;;                                   (if (= pro-label *verum*)
;;                                     (remove #(= *verum* %) con-labels)
;;                                     (conj (remove #(= *verum* %) con-labels)
;;                                       pro-label
;;                                       )
;;                                     )))]
;;           (cond (empty? collectedlabels) *verum*
;;             (= collectedlabels *falsum*) (recur (set (next pro-args)) labels)
;;             :else (recur (set (next pro-args))
;;                     (union labels collectedlabels))))))))

;; (defmethod ps-out-label :cce [ag asm s]
;;   {:pre [(set? asm)]}
;;   ;(println "computing pe-out-label for" s)
;;   (let [alpha 0.5
;;         beta 0.3
;;         pro-args (filter (fn [a] (> (sget a :weight) alpha)) (pro-arguments ag s))
;;         con-args (con-arguments ag s)
;;         l (loop [pro-args pro-args
;;                  labels #{}]
;;             (if (empty? pro-args)
;;               (if (empty? labels)
;;                 *verum*
;;                 (conj (combine-conjunction-of-dnf labels)
;;                     #{(statement-complement s)}))
;;               (let [pro-arg (first pro-args)
;;                     w (sget pro-arg :weight)
;;                     greater-cons (filter #(<= (- w (sget % :weight)) beta) con-args)
;;                     pro-label (argument-out-label ag asm pro-arg)
;;                     [con-labels con-labels-true con-labels-false]
;;                     (collect-labels-disj #(argument-in-label ag asm %)
;;                       greater-cons)]
;;                 (cond
;;                   (or (= pro-label *verum*) con-labels-true) (recur (set (next pro-args)) labels),
;;                   (and (= pro-label *falsum*) con-labels-false) #{#{(statement-complement s)}},
;;                   (= pro-label *falsum*) (recur (set (next pro-args))
;;                                             (conj labels con-labels)),
;;                   con-labels-false (recur (set (next pro-args))
;;                                      (conj labels pro-label)),
;;                   :else (recur (set (next pro-args))
;;                           (conj labels
;;                             (union pro-label con-labels)))))))]
;;     l))

;; (defmethod ps-in-label :brd [ag asm s]
;;   {:pre [(set? asm)]}
;;   (let [alpha 0.5
;;         beta 0.3
;;         gamma 0.2
;;         pro-args (filter (fn [a] (> (sget a :weight) alpha)) (pro-arguments ag s))
;;         con-args (con-arguments ag s)]
;;     (loop [pro-args pro-args
;;            labels #{}]
;;       (if (empty? pro-args)
;;         (conj labels #{s})
;;         (let [pro-arg (first pro-args)
;;               w (sget pro-arg :weight)
;;               greater-cons (filter #(or (<= (- w (sget % :weight)) beta) (> (sget % :weight) gamma)) con-args)
;;               pro-label (argument-in-label ag asm pro-arg)
;;               [con-labels _ onefalse] (collect-labels-conj-break
;;                                         #(argument-out-label ag asm %)
;;                                         greater-cons)
;;               collectedlabels (if onefalse
;;                                 *falsum*
;;                                 (combine-conjunction-of-dnf
;;                                   (if (= pro-label *verum*)
;;                                     (remove #(= *verum* %) con-labels)
;;                                     (conj (remove #(= *verum* %) con-labels)
;;                                       pro-label
;;                                       )
;;                                     )))]
;;           (cond (empty? collectedlabels) *verum*
;;             (= collectedlabels *falsum*) (recur (set (next pro-args)) labels)
;;             :else (recur (set (next pro-args))
;;                     (union labels collectedlabels))))))))

;; (defmethod ps-out-label :brd [ag asm s]
;;   {:pre [(set? asm)]}
;;   ;(println "computing pe-out-label for" s)
;;   (let [alpha 0.5
;;         beta 0.3
;;         gamma 0.2
;;         pro-args (filter (fn [a] (> (sget a :weight) alpha)) (pro-arguments ag s))
;;         con-args (con-arguments ag s)
;;         l (loop [pro-args pro-args
;;                  labels #{}]
;;             (if (empty? pro-args)
;;               (if (empty? labels)
;;                 *verum*
;;                 (conj (combine-conjunction-of-dnf labels)
;;                     #{(statement-complement s)}))
;;               (let [pro-arg (first pro-args)
;;                     w (sget pro-arg :weight)
;;                     greater-cons (filter #(or (<= (- w (sget % :weight)) beta) (> (sget % :weight) gamma))  con-args)
;;                     pro-label (argument-out-label ag asm pro-arg)
;;                     [con-labels con-labels-true con-labels-false]
;;                     (collect-labels-disj #(argument-in-label ag asm %)
;;                       greater-cons)]
;;                 (cond
;;                   (or (= pro-label *verum*) con-labels-true) (recur (set (next pro-args)) labels),
;;                   (and (= pro-label *falsum*) con-labels-false) #{#{(statement-complement s)}},
;;                   (= pro-label *falsum*) (recur (set (next pro-args))
;;                                             (conj labels con-labels)),
;;                   con-labels-false (recur (set (next pro-args))
;;                                      (conj labels pro-label)),
;;                   :else (recur (set (next pro-args))
;;                           (conj labels
;;                             (union pro-label con-labels)))))))]
;;     l))

;; (defn assume-assumed-statements
;;   [ag]
;;   (let [all-nodes (get-nodes ag),
;;         accepted-nodes (filter (fn [n] (= (:status n) :accepted)) all-nodes),
;;         rejected-nodes (filter (fn [n] (= (:status n) :rejected)) all-nodes)]
;;     (set (concat
;;            (map :statement accepted-nodes)
;;            (map statement-complement (map :statement rejected-nodes))))))

;; (defn make-minimal
;;   ([label]
;;      (reduce make-minimal label label))
;;   ([label position]
;;      (doall (filter (fn [p]
;;               ;(println "comparing positions" (map literal->str position) (map literal->str p) (= (union position p) p))
;;               (or (= position p) (not (subset? position p))))
;;                    label))))

;; ;;; confusion between depth and height meaning?
;; ;;; (see Mantis bug tracker and the position Assistant implementation)

;; (defn position-depth "Returns the depth of a position in an argument graph"
;;   [ag p]
;;   (apply + (map (fn [s]
;;                   (let [n (get-node ag s)]
;;                     (depth-in ag n)))
;;                 p)))

;; (defn position-height
;;   "Returns the height of a position in an argument graph"
;;   [ag p]
;;   (apply + (map
;;             (fn [s]
;;               (let [n (get-node ag s)]
;;                 (height-in ag n)))
;;             p)))


;; ;(def path "C:\\Users\\stb\\Documents\\Carneades Project\\carneades\\src\\CarneadesExamples\\src\\carneades\\examples\\open_source_licensing\\impact-full.xml")
;; ;;(def path "C:\\Users\\stb\\Desktop\\contract.xml")
;; ;(def i (lkif-import path))
;; ;
;; ;(def ag1 (first (:ags i)))
;; ;
;; ;(defn c [n s] (symbol (str n s)))
;; ;(def il "http://carneades.berlios.de/impact-licensing#")
;; ;(def oss "http://carneades.berlios.de/oss-licenses#")
;; ;(def goal1 (list (c oss "mayUseLicenseTemplate") (c il "CarneadesEngine") (c oss "GPL_Template")))
;; ;;(def goal1 "contract")
;; ;(def l (statement-in-label ag1 (assume-assumed-statements ag1) goal1))

;; ;(def l #{#{:a :b :c} #{:a :c} #{:a} #{:b} #{:c :d} #{:a :c :b :d}})
;; ;(def l #{#{:a :b :c} #{:a :c}})
