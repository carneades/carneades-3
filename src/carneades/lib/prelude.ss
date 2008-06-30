#!r6rs

(library
 
 (carneades lib prelude)
 
 (export
  begin0 and-let* cons*
  unspecified unspecified?
  add1 sub1
  inc! dec!
  fx1+ fx1-
  id)
 
 (import
  (rnrs base)
  (only (rnrs arithmetic fixnums)
        fx+ fx-))
 
 (define unspecified
   (let ((void (if #f #t)))
     (lambda () void)))
 
 (define unspecified?
   (let ((v (unspecified)))
     (lambda (void)
       (eq? void v))))
 
 ;; ;;;;;;;;;;;;;;
 ;; Basic Macros
 ;; ;;;;;;;;;;;;;;
 
 (define-syntax fx1-
   (syntax-rules ()
     ((fx1- x)
      (fx- x 1))))
   
 (define-syntax fx1+
   (syntax-rules ()
     ((fx1+ x)
      (fx+ 1 x))))
 
 (define-syntax add1
   (syntax-rules ()
     ((add1 x)
      (+ 1 x))))
 
 (define-syntax sub1
   (syntax-rules ()
     ((sub1 x)
      (- x 1))))
 
 (define-syntax inc!
   (syntax-rules ()
     ((inc! x)
      (set! x (+ 1 x)))))
 
 (define-syntax dec!
   (syntax-rules ()
     ((dec! x)
      (set! x (- x 1)))))
 
 (define-syntax begin0
   (syntax-rules ()
     ((begin0 e1 es ...)
      (let ((result e1))
        es ...
        result))))
 
 ;; misc functions
 (define id
   (lambda (x) x))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pulled from SXML distro and used here solely to support same. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (define-syntax and-let*
   (syntax-rules ()
     ((and-let* () body ...)
      (begin body ...))
     ((and-let* ((var expr) clauses ...) body ...)
      (let ((var expr))
  (if var (and-let* (clauses ...) body ...) #f)))
     ((and-let* ((expr) clauses ...) body ...)
      (if expr (and-let* (clauses ...) body ...) #f))
     ((and-let* (var clauses ...) body ...)
      (if var (and-let* (clauses ...) body ...) #f))))
 
 (define (cons* a1 a2 . rest)
   (if (null? rest)
      (cons a1 a2)
      (cons a1 (apply cons* (cons a2 rest)))))
 
 )