#!r6rs

(library
 
 (carneades lkif2)
 
 (export lkif-import
         lkif-export
         make-lkif-data lkif-data? lkif-data-sources lkif-data-context
         lkif-data-rulebase lkif-data-stages
         make-stage stage? stage-argument-graph stage-context
         make-source source? source-element source-uri)
 
 (import (rnrs)
         (carneades lkif2 lkif2-base)
         (carneades lkif2 lkif2-import)
         (carneades lkif2 lkif2-export))
 
 )
         