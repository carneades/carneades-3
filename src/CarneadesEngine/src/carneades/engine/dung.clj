;;; Copyright (c) 2012 Fraunhofer Gesellschaft
;;; Licensed under the EUPL V.1.1

(ns ^{:doc "Dung Abstract Argumentation Frameworks"}
  carneades.engine.dung)

(defn abstract-argument? [x]
  (or (symbol? x)
      (string? x)
      (keyword? x)))

(defrecord ArgumentationFramework
  [arguments  ; set of abstract arguments
   attacks])  ; map from an argument to the set of arguments attacking the argument

(defn make-argumentation-framework
  [args attacks]
  (ArgumentationFramework. args attacks))

(defn label? [x] (contains? #{:in :out :undecided} x))

(defn label 
  "map abstract-argument -> label"
  [labelling arg]
  (let [l (get labelling arg)]
    (if (nil? l)
      :undecided
      l)))

(defn grounded-labelling 
  "argumentation-framework -> map from argument ids to labels"
  [af]
  (letfn [; label an argument :in if all attackers of the argumetn are labelled :out
	  (in [l] (reduce (fn [l2 arg] 
			    (if (nil? (get l2 arg))
			      (if (every? (fn [attacker] (= (get l2 attacker) :out))
					  (get (:attacks af) arg))
				(assoc l2 arg :in) 
				l2)                
			      l2))                 
			  l
			  (:arguments af)))
	  ; label an argument :out if some attacker of the argument is labelled :in
	  (out [l] (reduce (fn [l2 arg] 
			     (if (nil? (get l2 arg))
			       (if (some (fn [attacker] (= (get l2 attacker) :in))
					 (get (:attacks af) arg))
				 (assoc l2 arg :out) 
				 l2)                 
			       l2))                   
			   l
			   (:arguments af)))
          (f [l1] 
	     (let [l2 (out (in l1))] 
	       (if (= l1 l2) l1 (recur l2))))]
    (f {})))








