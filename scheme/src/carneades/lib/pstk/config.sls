#!r6rs

(library 
 (carneades lib pstk config)
 
 (export *wish-program*)
 
 (import (rnrs base))

 (define *wish-program* "/usr/local/bin/tclsh8.5")
 
 )