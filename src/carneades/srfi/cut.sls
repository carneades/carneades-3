#!r6rs
(library (carneades srfi cut)
  (export cut cute)
  (import (rnrs) (carneades srfi private include-resolve))
  
  (include/resolve ("carneades" "srfi" "cut") "cut.scm")  
)
