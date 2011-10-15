;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1


(ns ^{:doc "Generators for arguments from argument graphs."}
  carneades.engine.argument-from-arguments
  (:use carneades.engine.utils
        carneades.engine.argument
        carneades.engine.unify
        carneades.engine.statement
        carneades.engine.response))

; type generator: statement substitutions  -> (seq-of response)

(defn generate-arguments-from-arguments
  "argument-graph -> generator"
  [ag1]
  (fn [goal subs]
    (reduce (fn [l arg]
              (let [subs2 (unify goal (argument-head arg) subs)]
                (if (not subs2)
                  l
                  (conj l (make-response subs2 #{} (assoc arg :id (gensym "a")))))))
            []    
            (arguments ag1))))


(defn generate-assumptions-from-in-statements
  "argument-graph -> generator"
  [ag1]
  (fn [goal subs]
    (reduce (fn [l stmt]
              (let [subs2 (unify goal stmt subs)]
                (if (not subs2)
                  l
                  (conj l (make-response subs2 #{goal} nil)))))
            []
            (in-statements ag1))))

(defn generate-substitutions-from-assumptions
  "argument-graph -> generator"
  [ag1]
  (fn [goal subs]
    (reduce (fn [l stmt]
              (let [subs2 (unify goal stmt subs)]
                (if (not subs2)
                  l
                  (conj l (make-response subs2 #{} nil)))))
            []
            (assumptions ag1))))

