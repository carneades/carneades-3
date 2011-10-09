;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.examples.hundeanmeldung
  (:use clojure.pprint
        carneades.engine.argument
        carneades.engine.argument-builtins
        carneades.engine.shell
        carneades.engine.rule
        carneades.mapcomponent.viewer))

(def rb1 
  (rulebase
   
   (rule verwaltungsleistung
	 (if (or (hundeanmeldung ?x)
	         (hundeabmeldung ?x))
	   (verwaltungsleistung ?x)))

   (rule person
	 (if (and (vorname ?p ?fn)
		(name ?p ?n))
	   (person ?p)))

   (rule hund
	 (if (and (rasse ?h ?r)
		(farbe ?h ?f)
		(name ?h ?n))
	   (hund ?h)))

   (rule hundeanmeldung
	 (if (and (person ?p)
	          (hund ?h)
		(eigentuemer ?h ?p)
		(hundeanmeldung-datum ?ha ?d))
	   (and (hundeanmeldung ?ha)
		(hundeanmeldung-person ?ha ?p)
		(hundeanmeldung-hund ?ha ?h))))

   (rule hundeabmeldung
	 (if (and (hundeanmeldung ?han)
		(hundeanmeldung-person ?han ?p)
		(hundeanmedlung-hund ?han ?h)
		(hundeabmeldung-datum ?hab ?d))
	   (and (hundeabmeldung ?hab)
		(hundeabmeldung-person ?hab ?p)
		(hundeabmeldung-hund ?hab ?h))))

   (rule hund-muss-neuangemeldet-werden
	 (if (and (hundeanmeldung-hund ?ha ?h)
		(hundeanmeldung-datum ?ha ?d1)
		(vor-mehr-als-drei-jahren ?d1))
	   (hund-muss-neuangemeldet-werden ?h)))
   ))

; accept some facts 
(def ag1
     (accept *empty-argument-graph*
	     '((vorname p1 "Christian")
	       (name p1 "Breitenstrom")
	       (rasse h1 "Australian Shepard")
	       (farbe h1 "Blue Merle")
	       (name h1 "Bennie")
	       (eigentuemer h1 p1)
	       (hundeanmeldung-datum ha1 d1)
	       (vor-mehr-als-drei-jahren d1))))
	       
(def engine (make-engine 100 1 ag1
                (list (generate-arguments-from-rules rb1) (builtins))))

(defn main []
  ;;(view (:arguments (first (solutions (engine '(hundeanmeldung ?ha))))))
  (view (:arguments
         (first (solutions (engine '(hund-muss-neuangemeldet-werden ?h)))))))
