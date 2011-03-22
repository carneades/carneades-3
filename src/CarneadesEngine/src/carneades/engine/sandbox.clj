;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns ^{:doc "OBSOLETE. This is not used anymore but could be used for security reason
            to sandbox the eval predicate of rules in the carneades engine
            when used on a server with inputs from users"}
    carneades.engine.sandbox
  (:use clojure.contrib.def
        ;; net.licenser.sandbox
        ))

;; (defvar- *sandbox* (new-sandbox))

(defn eval-expr [expr]
  "throws java.lang.SecurityException
   and java.util.concurrent.TimeoutException"
  (eval expr))

