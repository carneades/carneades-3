; MODULE DEFINITION FOR SRFI-27
; =============================
; 
; Sebastian.Egner@philips.com, Mar-2002, in Gambit 3.0
;
; This file contains the top-level definition for the 54-bit flonum
; implementation of SRFI-27 for the Gambit 3.0 system.
;
; 1. The core generator is implemented in 'mrg32k3a-c.scm'.
; 2. The generic parts of the interface are in 'mrg32k3a.scm'.
; 3. The non-generic parts (record type, time, error) are here.
;
; loading the module:
;   (load "srfi-27-c.scm")
;
; history of this file:
;   SE, 25-Mar-2002: initial version
;   SE, 10-Apr-2002: incorporated Brad Lucier's optimizations

(declare ; from Brad Lucier
 (standard-bindings)
 (extended-bindings)
 (block)
 (not safe))


(define-structure :random-source
  state-ref
  state-set!
  randomize!
  pseudo-randomize!
  make-integers
  make-reals)

(define :random-source-make 
  make-:random-source)

(define (:random-source-current-time)
  (real-time))

(include "mrg32k3a-c.scm")

(include "mrg32k3a.scm")
