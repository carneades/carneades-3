;; Copyright (c) 2010 Fraunhofer Gesellschaft
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns ^{:doc "Provide a function for evaluating Clojure code in a protected mode.
            This is not yet fully implemented and is not used yet elsewhere in Carneades. 
            A sandbox for evaluating Clojure code would be useful for the
            implementing a version of eval predicate, in the argument-builtins module,
            when evaluating code from users over a network."}
    carneades.engine.sandbox
  ;; (:use
  ;;       ;; net.licenser.sandbox
  ;;       )
  )

;; (defvar- *sandbox* (new-sandbox))

(defn eval-expr [expr]
  "throws java.lang.SecurityException
   and java.util.concurrent.TimeoutException"
  (eval expr) ;  TO DO: replace with the actual sandbox
  )

