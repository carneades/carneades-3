;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.engine.sandbox
  (:use clojure.contrib.def
        ;; net.licenser.sandbox
        ))

;; (defvar- *sandbox* (new-sandbox))

(defn eval-expr [expr]
  "throws java.lang.SecurityException
   and java.util.concurrent.TimeoutException"
  (eval expr))

