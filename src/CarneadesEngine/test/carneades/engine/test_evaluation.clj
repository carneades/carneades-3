;;; Copyright (c) 2011 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1


(ns carneades.engine.test-evaluation
  (:use clojure.test
        carneades.engine.statement
        carneades.engine.argument
        carneades.engine.scheme
        carneades.engine.argument-graph
        carneades.engine.argument-construction
        carneades.engine.argument-evaluation
        carneades.engine.caes
        carneades.engine.shell))


; Example argument graphs to test whether arguments are being evaluated properly.

; Benchmark 1. The Tandem example
; Source:
; Baroni, P., Caminada, M., and Giacomin, M. An introduction to argumentation semantics. 
; The Knowledge Engineering Review 00, 0 (2004), 1-24.

(def jw (make-statement :text {:en "John wants to ride on the tandem."}))
(def mw (make-statement :text {:en "Mary wants to ride on the tandem."}))
(def sw (make-statement :text {:en "Suzy wants to ride on the tandem."}))
(def jt (make-statement :text {:en "John is riding on the tandem."}))
(def mt (make-statement :text {:en "Mary is riding on the tandem."}))
(def st (make-statement :text {:en "Suzy is riding on the tandem."}))
(def bottom (make-statement :text {:en "The claims are inconsistent."}))


(def A1 (make-argument :conclusion bottom :premises [jt, mt, st]))   
(def A5 (make-argument :conclusion jt :premises [jw]))
(def A6 (make-argument :conclusion mt :premises [mw]))
(def A7 (make-argument :conclusion st :premises [st]))

(def ag1 
  (-> (make-argument-graph)
      (assert-arguments [A1, A5, A6, A7])
      (assume [jw, mw, st])))

; Using the CAES evaluator, bottom is in. Consistency
; can be restored by using abduction to find minimal
; changes to the argument graph which make bottom out.
(deftest test-tandem-carneades
   (is (in? (evaluate carneades-evaluator ag1) (:atom bottom))))


; The following examples are from:
; Prakken, H. An abstract framework for argumentation with structured arguments. 
; Argument & Computation 1, (2010), 93-124.

(def frisian (make-statement :text {:en "Wiebe is Frisian."}))
(def dutch (make-statement :text {:en "Wiebe is Dutch."}))
(def tall (make-statement :text {:en "Wiebe is Tall."}))

(def A1 (make-argument :strict true :conclusion frisian :premises [dutch]))
(def A2 (make-argument :conclusion tall :premises [dutch]))

(def ag2 
  (-> (make-argument-graph)
      (assert-arguments [A1, A2])
      (accept [frisian])))

(deftest test-frisian-carneades
   (is (in? (evaluate carneades-evaluator ag2) (:atom tall))))

;; The next example shows how arguments can be constructed by instantiating schemes.

(def theory1 
  (make-theory 
    :sections
    [(make-section
      :schemes
        [(make-scheme 
          :name "Expert Witness Testimony"
          :conclusions ['?P]
          :premises {:major '(expert ?E ?D), 
                     :minor '(asserts ?E ?P)}
          :exceptions {:reliable '(reliable-as-source ?E),
                       :consistent '(consistent-with-other-witnesses ?P)}
          :assumptions {:credible '(credible-expert ?E),
                        :backup-evidence '(based-on-evidence ?E)})])]))

(def expert (make-statement :atom '(expert Joe Dentistry)
                            :text {:en "Joe is an expert dentist."}))
(def assertion (make-statement :atom '(asserts Joe (has-cavities Susan))))

(def max-goals 500)  
(def generators (list (generate-arguments-from-theory theory1)))   
(def assumptions2 (map statement->literal (list expert assertion)))   
(def query '(has-cavities ?x))           

(def ag3 (construct-arguments (make-argument-graph) query max-goals assumptions2 generators))

(deftest test-expert-witness-carneades
   (is (in? (evaluate carneades-evaluator ag3) '(has-cavities Susan))))




                    
    
  
  

