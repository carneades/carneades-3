#!r6rs

(library
 
 (carneades lkif2 lkif2-base)
 
 (export make-lkif-data lkif-data? lkif-data-sources lkif-data-context
         lkif-data-rulebase lkif-data-stages
         make-stage stage? stage-argument-graph stage-context
         make-source source? source-element source-uri)
 
 (import (rnrs))
 
 
 ; --------------------------------
 ; datatypes definitions
 
 ; lkif-import is the datastructure that is supposed to
 ; be returned by the import function
 (define-record-type lkif-data
   (fields sources            ; list of sources
           context            ; context build by theory axioms
           rulebase           ; rulebase build by theory rules
           stages))           ; list of stages
 
 (define-record-type stage
   (fields argument-graph     ; argument-graph
           context))          ; context for argument-graph
 
 ; source in lkif2
 (define-record-type source
   (fields element
           uri))
 
 )