; MODULE DEFINITION FOR SRFI-27, C/SCHEME-IMPLEMENTATION
; ======================================================
; 
; Sebastian.Egner@philips.com, Mar-2002, in Scheme 48 0.57
;
; This file contains the top-level definition for the C-code
; implementation of SRFI-27 for the Scheme 48 0.57 system. 
;
; 1. The core generator is implemented in 'mrg32k3a-b.c'.
; 2. The generic parts of the interface are in 'mrg32k3a.scm'.
; 3. The non-generic parts (record type, time, error, C-bindings) are here.
;
; creating the module:
;   copy mrg32k3a-b.c into $SCHEME48/c/srfi-27/mrg32k3a-b.c
;   edit $SCHEME48/Makefile.in
;      add c/srfi-27/mrg32k3a-b.o to EXTERNAL_OBJECTS
;      add mrg32k3a_init to EXTERNAL_INITIALIZERS
;      add the make line c/srfi-27/mrg32k3a-b.o: c/scheme48.h
;   cd $SCHEME48
;   make clean
;   configure
;   make
;   cd $SRFI27
;   ,config ,load srfi-27-b.scm
;
; loading the module, once created:
;   ,open srfi-27
;
; history of this file:
;   SE, 22-Mar-2002: initial version
;   SE, 25-Mar-2002: initial version

(define-structure srfi-27
  (export 
   random-integer
   random-real
   default-random-source
   make-random-source
   random-source?
   random-source-state-ref
   random-source-state-set!
   random-source-randomize!
   random-source-pseudo-randomize!
   random-source-make-integers
   random-source-make-reals)

  (open
   scheme-level-1
   floatnums
   external-calls
   (subset srfi-9 (define-record-type)) 
   (subset srfi-23 (error))
   (subset posix-time (current-time))
   (subset posix (time-seconds)))

  (begin
    (define-record-type :random-source
      (:random-source-make
       state-ref
       state-set!
       randomize!
       pseudo-randomize!
       make-integers
       make-reals)
      :random-source?
      (state-ref :random-source-state-ref)
      (state-set! :random-source-state-set!)
      (randomize! :random-source-randomize!)
      (pseudo-randomize! :random-source-pseudo-randomize!)
      (make-integers :random-source-make-integers)
      (make-reals :random-source-make-reals))

    (define (:random-source-current-time)
      (time-seconds (current-time)))

    ; interface to core generator

    (import-lambda-definition mrg32k3a-pack-state1 (state))
    (import-lambda-definition mrg32k3a-unpack-state1 (state))
    (import-lambda-definition mrg32k3a-random-range ())
    (import-lambda-definition mrg32k3a-random-integer (state range))
    (import-lambda-definition mrg32k3a-random-real (state))

    (define (mrg32k3a-pack-state state)
      (mrg32k3a-pack-state1
       (list->vector
        (apply append 
               (map (lambda (x) 
                      (list (modulo x 65536) 
                            (quotient x 65536))) 
                    (vector->list state))))))

    (define (mrg32k3a-unpack-state state)
      (let ((s (mrg32k3a-unpack-state1 state)) (w 65536))
        (vector
         (+ (vector-ref s  0) (* (vector-ref s  1) w))
         (+ (vector-ref s  2) (* (vector-ref s  3) w))
         (+ (vector-ref s  4) (* (vector-ref s  5) w))
         (+ (vector-ref s  6) (* (vector-ref s  7) w))
         (+ (vector-ref s  8) (* (vector-ref s  9) w))
         (+ (vector-ref s 10) (* (vector-ref s 11) w))))))

  (files "mrg32k3a.scm"))
