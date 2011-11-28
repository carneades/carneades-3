;;; Copyright (c) 2011 Fraunhofer Gesellschaft
;;; Licensed under the EUPL V.1.1

(ns ^{:doc "Theories defined using argumentation schemes."}
  carneades.engine.scheme
            
  (:use 
    carneades.engine.statement
    carneades.engine.unify
    carneades.engine.argument
    carneades.engine.argument-generator
    carneades.engine.dublin-core
    [carneades.engine.utils :only (mapinterleave)]))

(defrecord Form
  [positive     ; string for the positive sentences
   negative     ; string for the negative sentences
   question])   ; string for questions

(defn make-form 
  "key value ... -> form"
  [& key-values]  
  (merge (Form. 
           ""    ; positive
           ""    ; negative
           "")   ; question
         (apply hash-map key-values)))

(defn form? [x] (instance? Form x))
  
(defrecord Predicate 
  [symbol   ; symbol
   arity    ; integer
   forms])  ; (lang -> form) map, where lang is one of the keywords :en, :de, etc.

(defn make-predicate
  "key value ... -> predicate"
  [& key-values]  
  (merge (Predicate. 
           (gensym "p")    ; symbol
           0               ; arity
           {})             ; forms
         (apply hash-map key-values)))

(defn predicate? [x] (instance? Predicate x))

(defrecord Individual
  [symbol   ; symbol
   text])   ; (lang -> string) map.

(defn make-individual
  "key value ... -> predicate"
  [& key-values]  
  (merge (Individual. 
           (gensym "p")    ; symbol
           {})             ; text map
         (apply hash-map key-values)))

(defn individual? [x] (instance? Individual x))

; variables are allowed as conclusions of schemes to enable them
; to represent argumentation schemes, such as arguments from 
; expert witness testimony, whose conclusions can be any (literal) proposition.

(defrecord Scheme
  [id            ; symbol
   header        ; nil or dublin metadata
   conclusion    ; atom or variables (ranging over literals)
   pro           ; boolean; generates con arguments if false
   strict        ; boolean, defeasible if false
   weight        ; nil or number in the range 0.0 to 1.0
   premises      ; premise sequence 
   exceptions    ; premise sequence
   assumptions]) ; premise sequence
    
; The scope of scheme ids is local to their section in the theory. 

(defn make-scheme
  "key value ... -> scheme"
  [& key-values]  
  (let [m  (-> (merge (Scheme. 
                        (gensym "s")    ; id 
                        nil             ; header
                        nil             ; conclusion
                        true            ; pro
                        false           ; strict
                        0.5             ; weight
                        []              ; premises
                        []              ; exceptions
                        [])             ; assumptions
                      (apply hash-map key-values)))]
    ; normalize the conclusion and direction of the scheme
    (assoc m :conclusion (literal-atom (:conclusion m))
           :pro  (or (and (literal-pos? (:conclusion m))
                          (:pro m))
                     (and (literal-neg? (:conclusion m))
                          (not (:pro m)))))))

(defn scheme? [x] (instance? Scheme x))

(defn scheme-conclusion-literal
  "scheme -> literal
   Returns the conclusion of the scheme as a positive 
   literal, if the scheme is pro, or negative literal,
   if the scheme gument is con."
  [s]
  {:pre [(scheme? s)]}
  (if (:pro s) 
    (:conclusion s) 
    (literal-complement (:conclusion s))))


; When applying schemes, undercutters are generated from the exceptions of schemes, 
; where the undercutters are arguments with the form:
; (make-argument 
;  :conclusion '(undercut <arg-id>)
;  :premises [<exception>])


(defn instantiate-scheme
  "scheme substitutions -> scheme
   Instantiate or partially instantiate a scheme by substituting
   variables in the scheme with their values in the map."
  [scheme subs]
  {:pre [(scheme? scheme) (map? subs)]}
  (letfn [(apply-subs [literal] (apply-substitutions subs literal))]
    (assoc scheme
           :conclusion (apply-subs (:conclusion scheme)),
           :premises (map (fn [p] (assoc p :statement (apply-subs (:statement p))))
                          (:premises scheme)),
           :exceptions (map (fn [p] (assoc p :statement (apply-subs (:statement p))))
                            (:exceptions scheme)),        
           :assumptions (map (fn [p] (assoc p :statement (apply-subs (:statement p))))
                             (:assumptions scheme)))))
  
(defn axiom 
  "literal -> scheme"
  [literal]
  (make-scheme 
    :strict true 
    :conclusion (literal-atom literal) 
    :pro (literal-pos? literal)))

; The scope of section ids is their theory

(defrecord Section
  [id          ; symbol
   header      ; nil or a dublin core metadata structure about this model
   schemes     ; sequence of schemes
   sections])  ; sequence of sections; i.e. subsections    

(defn make-section
  "key value ... -> section"
  [& key-values]  
  (merge (Section. 
           (gensym "?")    ; id
           nil             ; header
           []              ; schemes
           [])             ; (sub)sections
         (apply hash-map key-values)))

(defn section? [x] (instance? Section x))

(defn- rename-variables-in-premises
  "substitutions (seq-of premise) -> [substitutions (seq-of premise)]"
  [subs premises]
  (reduce (fn [pair p]
            (let [[subs1 atom] (rename-variables (first pair) (:statement p))]
              [subs1 (conj (second pair) (assoc p :statement atom))]))
          [subs []]
          premises))

(defn- rename-scheme-variables [scheme]
  (let [[m1 conclusion] (rename-variables {} (:conclusion scheme)),
        [m2 premises] (rename-variables-in-premises m1 (:premises scheme)),
        [m3 exceptions] (rename-variables-in-premises m2 (:exceptions scheme)),
        [m4 assumptions] (rename-variables-in-premises m3 (:assumptions scheme))]
    (assoc scheme 
           :conclusion conclusion
           :premises premises
           :exceptions exceptions
           :assumptions assumptions)))

(defrecord Theory
  [header     ; nil or a dublin core metadata structure about the model.
   language   ; (symbol -> individual or predicate) map
   schemes    ; scheme sequence
   sections]) ; section sequence
 
(defn make-theory
  "key value ... -> theory"
  [& key-values]  
  (merge (Theory. 
           nil             ; header
           {}              ; language
           []              ; schemes
           [])             ; sections
         (apply hash-map key-values)))

(defn theory? [x] (instance? Theory x))
   
(defn- scheme-index-key 
  "term -> symbol 
   Returns the symbol used to index a scheme by its conclusions 
   for quicker retrieval. By default schemes are indexed under :other."
  [trm]
  {:pre [(term? trm)]}
  ; (println "term: " trm)
  (cond (constant? trm) trm,
        (variable? trm) :other,
        (literal? trm) (literal-predicate trm),
        (compound-term? trm) (term-functor trm)
        :else :other))
 
(defn create-scheme-index
  "map section-or-theory -> (symbol -> seq of schemes) map"
  [map1 part]
  ; (println "section: " section)
  (let [map2 (reduce (fn [map2 scheme]
                       ; (println "scheme: " scheme)
                         (assoc map2
                            (scheme-index-key (:conclusion scheme))
                                (conj (get map2 (scheme-index-key (:conclusion scheme)))
                                      scheme)))      
                     map1
                     (:schemes part))]
    (reduce (fn [map3 section]
              (create-scheme-index map3 section))
            map2
            (:sections part))))

(defn get-schemes 
  "map goal substititions -> sequence of schemes
   where the goal is a literal. Does not return
   :other schemes, since they match every goal
   and are not well controlled when using with backward 
   chaining."
  [index goal subs]
  {:pre [(map? index) 
         (literal? goal)
         (map? subs)]}
  (let [key (scheme-index-key (apply-substitutions subs goal))]
    (if (= key :other)
      ()
      (concat (get index key)))))

(defn apply-scheme
  "scheme literal substitutions -> seq-of response"
  [scheme goal subs]
  {:pre [(scheme? scheme) (literal? goal) (map? subs)]}
  (letfn [(apply-for-conclusion
            [scheme c]
            ;; apply the scheme for conclusion c
            (let [subs2 (or (unify c goal subs)
                            (unify `(~'applies ~(:id scheme) ~c) goal subs))]
              (if (not subs2)
                false ; fail
                (let [id (gensym "a")]
                  (cons (make-response subs2
                                       (map (fn [p] (if (:positive p)
                                                        (:statement p)
                                                        (literal-complement (:statement p))))
                                            (:assumptions scheme))
                                       (make-argument 
                                         :id id,
                                         :conclusion goal,
                                         :strict (:strict scheme),
                                         :weight (:weight scheme),
                                         :premises (concat (:premises scheme)
                                                           (:assumptions scheme)),
                                         :scheme (:id scheme)))
                        (map (fn [e] (make-response subs2
                                                    ()
                                                    (make-argument 
                                                      :conclusion `(~'undercut ~id)
                                                      :strict false
                                                      :weight (:weight scheme)
                                                      :premises [e]
                                                      :scheme (:id scheme))))
                             (:exceptions scheme)))))))]
    (apply concat (filter identity 
                          (map #(apply-for-conclusion scheme %) 
                               (list (scheme-conclusion-literal scheme)))))))


;; Generators for arguments from schemes and theories:

(defn generate-arguments-from-scheme
  "scheme -> argument-generator"
  [scheme]
  (reify ArgumentGenerator
    (generate [this goal subs]
       (apply-scheme scheme goal subs))))

(defn generate-arguments-from-theory
  "theory -> argument-generator"
  [theory1]
  (let [index (create-scheme-index {} theory1)]
    (reify ArgumentGenerator
      (generate [this goal subs]
        (mapinterleave
          (fn [s] (apply-scheme s goal subs))
          (map rename-scheme-variables (get-schemes index goal subs)))))))

