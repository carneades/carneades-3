#!r6rs

(library
 (carneades lib pstk system)
 
 (export run-program keyword? keyword->string)
 (import (rnrs base))
 
 (define (err who)
   (error who "Some PSTK system features were not implemented for \
     this platform.  You need to supply an implentation-specific \
     (carneades lib pstk system) library for PSTK to work in \
     your system.  See system.ikarus.sls, system.mzscheme.sls, \
     system.ypsilon.sls files for examples.  If you run have \
     a compatibility library implemented for a new platform, \
     please consider contributing it to the main PSTK distribution."))
 
 (define (run-program program) (err 'run-program))
 
 (define (keyword? x) (err 'keyword?))
 
 (define (keyword->string kwd) (err 'keyword->string))
 
 (err #f)
)
