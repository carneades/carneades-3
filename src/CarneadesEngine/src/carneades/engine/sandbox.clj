;;; Copyright (c) 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns ^{:doc "Provide a function for evaluating Clojure code in a protected mode.
            This is not yet fully implemented and is not used yet elsewhere in Carneades. 
            A sandbox for evaluating Clojure code would be useful for the
            implementing a version of eval predicate, in the argument-builtins module,
            when evaluating code from users over a network."}
    carneades.engine.sandbox
  (:use clojure.contrib.def
        ;; net.licenser.sandbox
        ))

;; (defvar- *sandbox* (new-sandbox))

(defn eval-expr [expr]
  "throws java.lang.SecurityException
   and java.util.concurrent.TimeoutException"
  (eval expr) ;  TO DO: replace with the actual sandbox
  )

