#!r6rs

(library
 
 (carneades lkif2)
 
 (export lkif-import
         lkif-export
         lkif?
         make-lkif-data lkif-data? lkif-data-sources lkif-data-argument-graphs
         lkif-data-rulebase
         make-source source? source-element source-uri)
 
 (import (rnrs)
         (carneades lkif2 lkif2-base)
         (carneades lkif2 lkif2-import)
         (carneades lkif2 lkif2-export))
 
 )
         