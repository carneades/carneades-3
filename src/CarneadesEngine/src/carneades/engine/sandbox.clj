;;; Copyright (c) 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns ^{:doc "This is not used yet but could be useful for the
            eval predicate of rules in the carneades engine
            when used on a server with inputs from users over the network."}
    carneades.engine.sandbox
  (:use clojure.contrib.def
        ;; net.licenser.sandbox
        ))

;; (defvar- *sandbox* (new-sandbox))

(defn eval-expr [expr]
  "throws java.lang.SecurityException
   and java.util.concurrent.TimeoutException"
  (eval expr))

