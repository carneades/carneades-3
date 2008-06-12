(module stream mzscheme
  ; The SRFI 40 implementation of streams extended with 
  ; other stream functions from Abelson and Sussman's 
  ; "Structure and Interpretation of Computer Programs"
  
  (require (lib "stream.ss" "srfi" "40"))
  (provide (all-from (lib "stream.ss" "srfi" "40")))
  (provide (all-defined))
  
  ; stream-append: stream stream -> stream
  (define (stream-append s1 s2)
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
  
  ; stream->list: (stream-of x) -> (list-of x)
  ; The value of (stream->list s) is undefined is s is infinite
  (define (stream->list s)
    (if (stream-null? s) 
        null
        (cons (stream-car s) (stream->list (stream-cdr s)))))
  
  ) ; end of stream module