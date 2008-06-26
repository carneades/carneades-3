;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Routines used in the "reset" to a common base to support multiple scheme impls. ;;
;; Clean this up as the common base line for RL3 is R6RS.			   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; FIXME this should all go away. RPR

(library
 (carneades lib  xml sxml env)
 
 (export nl cout cerr)
 
 (import 
  (rnrs base)
  (only (rnrs io simple)
	display 
	current-error-port))
 
 (define nl (string #\newline))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; like cout << arguments << args				   ;;
;; where argument can be any Scheme object. If it's a procedure	   ;;
;; (without args) it's executed rather than printed (like newline) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
 (define (cout . args)
   (for-each (lambda (x)
	       (if (procedure? x) (x) (display x)))
	     args))
 
 (define cerr 
   (lambda args
     (for-each (lambda (x)
		 (if (procedure? x)
		    (x (current-error-port))
		    (display x (current-error-port))))
	       args)))
 
 )



