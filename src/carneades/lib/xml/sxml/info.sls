#!r6rs

(library
  (carneades lib xml sxml info)
  
  (export
   name)
  
  (import
   (rnrs base))
  
  (define name "sxml")

  (define blurb
    (list "Collection of tools for processing markup documents "
          "in the form of S-expressions"))
  
  (define primary-file "sxml.ss")
  
  (define doc.txt "doc.txt")
  
  (define homepage "http://modis.ispras.ru/Lizorkin/sxml-tutorial.html")
  
  (define categories '(xml)))
