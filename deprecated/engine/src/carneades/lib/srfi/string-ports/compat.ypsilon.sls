#!r6rs

(library 
 
 (carneades lib srfi string-ports compat)
 
  (export open-output-string
          get-output-string)
  
  (import (only (srfi srfi-6) open-output-string get-output-string)))
