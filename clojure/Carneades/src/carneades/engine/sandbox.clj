(ns carneades.engine.sandbox
  (:use clojure.contrib.def
        net.licenser.sandbox))

(defvar- *sandbox* (new-sandbox))

(defn eval-expr [expr]
  "throws java.lang.SecurityException"
  (*sandbox* expr))

