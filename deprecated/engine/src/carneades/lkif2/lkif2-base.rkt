#!r6rs

(library
 
 (carneades lkif2 lkif2-base)
 
 (export make-lkif-data lkif-data? lkif-data-sources lkif-data-rulebase
         lkif-data-argument-graphs
         make-source source? source-element source-uri
         make-statement statement? statement-id statement-value statement-assumption statement-standard statement-atom)
 
 (import (rnrs))
 
 
 ; --------------------------------
 ; datatypes definitions
 
 ; lkif-import is the datastructure that is supposed to
 ; be returned by the import function
 (define-record-type lkif-data
   (fields sources            ; list of sources
           rulebase           ; rulebase build by theory
           argument-graphs))  ; list of argument-graphs
 
 ; source in lkif2
 (define-record-type source
   (fields element
           uri)) 
 
  ; statement in lkif2
 (define-record-type statement
   (fields id
           value
           assumption
           standard
           atom))
 
 )