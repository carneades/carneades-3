#!r6rs

(library (streams)
;(module stream mzscheme
         
        
         ; The SRFI 40 implementation of streams extended with 
         ; other stream functions from Abelson and Sussman's 
         ; "Structure and Interpretation of Computer Programs"
         
       #|(require (lib "stream.ss" "srfi" "40"))
         (provide (all-from (lib "stream.ss" "srfi" "40")))
         (provide (all-defined))|#
         
         (export stream-append stream-interleave stream-accumulate
                 stream-flatten stream-flatmap stream->list)
         (import
          (rnrs base)
          ;(srfi/40 stream))  ; TODO: use SRFI 41, when available
          (carneades streams))
         
         ; already defined in streams derived
         ; stream-append: stream stream -> stream
         #;(define (stream-append s1 s2)
           (if (stream-null? s1) 
               s2
               (stream-cons (stream-car s1)
                            (stream-append (stream-cdr s1) s2))))
         
         (define (stream-interleave s1 s2)
           (if (stream-null? s1)
               s2
               (stream-cons (stream-car s1)
                            (stream-delay (stream-interleave s2 (stream-delay (stream-cdr s1)))))))
         
         (define (stream-accumulate combiner initial-value stream)
           (if (stream-null? stream)
               initial-value
               (combiner (stream-car stream)
                         (stream-delay (stream-accumulate combiner
                                                          initial-value
                                                          (stream-delay (stream-cdr stream)))))))
         
         (define (stream-flatten stream)
           (stream-accumulate stream-interleave stream-null stream))
         
         (define (stream-flatmap f s) (stream-flatten (stream-map f s)))
         
         ; already defined in streams derived
         ; stream->list: (stream-of x) -> (list-of x)
         ; The value of (stream->list s) is undefined is s is infinite
         #;(define (stream->list s)
           (if (stream-null? s) 
               '()
               (cons (stream-car s) (stream->list (stream-cdr s)))))
         
         ) ; end of stream library