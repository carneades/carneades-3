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
   name          ; string
   conclusions   ; sequence of literals or variables (ranging over literals)
   strict        ; boolean, defeasible if false
   weight        ; nil or number in the range 0.0 to 1.0
   premises      ; (keyword -> literal) map, where the keyword is the role of each premise
   exceptions    ; (keyword -> literal) map
   assumptions   ; (keyword -> literal) map
   section])     ; the section to which this scheme belongs

(defn- pvector->pmap
  "assures that a property is a map, by converting vectors to maps if necessary."
  [scheme property-key]
  (let [property (property-key scheme)]
    (print "property " property)
    (if (map? property)
      scheme
      (assoc scheme property-key 
             (zipmap (map (fn [i] (gensym "p"))
                          (range (count property)))
                     property)))))
    
; The scope of scheme ids is local to their section in the theory. 

(defn make-scheme
  "key value ... -> scheme"
  [& key-values]  
  (-> (merge (Scheme. 
               (gensym "c")    ; id 
               ""              ; name
               []              ; conclusions
               false           ; strict
               0.5             ; weight
               {}              ; premises
               {}              ; exceptions
               {}              ; assumptions
               nil)            ; the section of this scheme
             (apply hash-map key-values))
      (pvector->pmap :premises)
      (pvector->pmap :exceptions)
      (pvector->pmap :assumptions)))

(defn scheme? [x] (instance? Scheme x))

; When applying schemes, undercutters are generated from the exceptions of schemes, 
; where the undercutters are arguments with the form:
; (make-argument 
;  :conclusion (literal->statement '(undercut <arg-id>))
;  :premises [(literal->statement <exception>)])


(defn instantiate-scheme
  "scheme substitutions -> scheme
   Instantiate or partially instantiate a scheme by substituting
   variables in the scheme with their values in the map."
  [scheme subs]
  {:pre [(scheme? scheme) (map? subs)]}
  (letfn [(apply-subs [literal] (apply-substitutions subs literal))]
    (assoc scheme
           :conclusions (vec (map apply-subs (:conclusions scheme))),
           :premises (zipmap (keys (:premises scheme))
                             (map apply-subs (vals (:premises scheme)))),
           :exceptions (zipmap (keys (:exceptions scheme))
                               (map apply-subs (vals (:exceptions scheme)))),        
           :assumptions (zipmap (keys (:assumptions scheme))
                                (map apply-subs (vals (:assumptions scheme)))))))
  
(defn axiom 
  "literal -> scheme"
  [literal]
  (make-scheme :strict true :conclusions [literal]))

; The scope of section ids is their theory

(defrecord Section
  [id          ; symbol
   name        ; string
   schemes     ; sequence of schemes
   source])    ; source or nil

(defn make-section
  "key value ... -> section"
  [& key-values]  
  (merge (Section. 
           (gensym "?")    ; id
           ""              ; name
           []              ; schemes
           nil)            ; source
         (apply hash-map key-values)))

(defn section? [x] (instance? Section x))

(defn rename-scheme-variables [c]
  (let [[m1 conclusions] (rename-variables {} (seq (:conclusions c))),
        [m2 premise-vals] (rename-variables m1 (vals (:premises c))),
        [m3 exception-vals] (rename-variables m2 (seq (vals (:exceptions c)))),
        [m4 assumption-vals] (rename-variables m3 (seq (vals (:assumptions c))))]
    (assoc c 
           :conclusions conclusions
           :premises (zipmap (keys (:premises c)) premise-vals)
           :exceptions (zipmap (keys (:premises c)) exception-vals)
           :assumptions (zipmap (keys (:premises c)) assumption-vals))))

(defrecord Theory
  [name      ; string
   source    ; source
   language  ; (symbol -> individual or predicate) map
   sections  ; section sequence
   index])   ; (symbol -> seq of scheme) map; see index-key 
 
(defn make-theory
  "key value ... -> theory"
  [& key-values]  
  (merge (Theory. 
           ""              ; name
           nil             ; source
           {}              ; language
           []              ; sections
           {})             ; index
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
        (compound-term? trm) (term-functor trm)
        (statement? trm) (statement-predicate trm)
        :else :other))
 
(defn create-index
  "theory -> theory
   Create an index of the schemes in the theory, to enable more 
   efficient retrieval when constructing arguments."
  [theory1]
  {:pre [(theory? theory1)]}
  (reduce (fn [theory2 section]
            ; (println "section: " section)
            (reduce (fn [theory3 scheme]
                      ; (println "scheme: " scheme)
                      (reduce (fn [theory4 conclusion]
                                ; (println "conclusion: " conclusion)
                                (assoc theory4 
                                       :index
                                       (assoc (:index theory4)
                                              (scheme-index-key conclusion)
                                              (conj (get (:index theory4)
                                                         (scheme-index-key conclusion))
                                                    (assoc scheme :section section)))))
                              theory3
                              (:conclusions scheme)))
                    theory2
                    (:schemes section)))
          theory1
          (:sections theory1)))

(defn get-schemes 
  "theory goal substititions -> sequence of schemes
   where the goal is a literal. Does not return
   :other schemes, since they match every goal
   and are not well controlled when using with backward 
   chaining."
  [theory goal subs]
  {:pre [(theory? theory) 
         (literal? goal)
         (map? subs)]}
  (let [key (scheme-index-key (apply-substitutions subs goal))]
    (if (= key :other)
      ()
      (concat (get (:index theory) key)))))

(defn- scheme-theory-id
  "Returns an id for a scheme which has theory scope. 
   The id is constructed by joining the id of the section of
   the scheme with the local id of the scheme in the section.
   The id is used to reify schemes in the theory for
   use in, e.g., (applies ...) and (undercut ...) atoms."
  [scheme]
  (symbol (str (:id (:section scheme)) "." (:id scheme)))) 

(defn apply-scheme
  "scheme literal substitutions -> seq-of response"
  [scheme goal subs]
  {:pre [(scheme? scheme) (literal? goal) (map? subs)]}
  (letfn [(apply-for-conclusion
            [scheme c]
            ;; apply the scheme for conclusion c
            (let [subs2 (or (unify c goal subs)
                            (unify `(~'applies ~(scheme-theory-id scheme) ~c) goal subs))]
              (if (not subs2)
                false ; fail
                (let [id (gensym "a")]
                  (cons (make-response subs2
                                       (map literal->statement (vals (:assumptions scheme)))
                                       (make-argument 
                                         :id id
                                         :conclusion (literal->statement goal)
                                         :strict (:strict scheme)
                                         :weight (:weight scheme)
                                         :premises (zipmap (keys (:premises scheme))
                                                           (map literal->statement 
                                                                (vals (:premises scheme))))
                                         :scheme (:name scheme)))
                        (map (fn [e] (make-response subs2
                                                    ()
                                                    (make-argument 
                                                      :conclusion (literal->statement  `(~'undercut ~id))
                                                      :strict false
                                                      :weight (:weight scheme)
                                                      :premises [(literal->statement e)]
                                                      :scheme (:name scheme))))
                             (vals (:exceptions scheme))))))))]
    (apply concat (filter identity 
                          (map #(apply-for-conclusion scheme %) 
                               (:conclusions scheme))))))


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
  (let [theory2 (create-index theory1)]
    (reify ArgumentGenerator
      (generate [this goal subs]
        (mapinterleave
          (fn [s] (apply-scheme s goal subs))
          (map rename-scheme-variables (get-schemes theory2 goal subs)))))))

