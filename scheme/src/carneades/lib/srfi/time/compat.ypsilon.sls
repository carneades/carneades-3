#!r6rs

(library 
 
 (carneades lib srfi time compat)
 
 (export format
         host:time-resolution
         host:current-time 
         host:time-nanosecond 
         host:time-second 
         host:time-gmt-offset)
 
 (import (rnrs base)
         (core))
 
 ;; Ypsilon uses microseconds,
 ;; so our resolution is 1000 nanoseconds
 (define host:time-resolution 1000)
 
 (define (host:current-time)
   (exact (floor (microsecond))))
 
 (define (host:time-nanosecond t)
   (* (mod t 1000000) 1000))
 
 (define (host:time-second t)
   (div t 1000000))
 
 (define (host:time-gmt-offset t)
   ;(date-time-zone-offset (seconds->date (host:time-second t))))
   7200) ; HACK: Timezone UTC+2 (CEST/CEDT)
 
 )
