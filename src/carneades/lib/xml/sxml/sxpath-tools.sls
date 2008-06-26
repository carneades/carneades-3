#| Utilty and helper procdedures for common sxpath operations 
   Author: Ray Racine

 Note: The whole sxml code set needs to be rationalized and documented.
       Temporary file for these until it can be refactored.
|#

(library
 (carneades lib xml sxml sxpath-tools)

 (export 
  select-single-node-text)

 (import
  (rnrs base)
  (only (carneades lib xml sxml sxpath)
	sxpath)
  (only (carneades lib xml sxml sxml-tools)
	sxml:text))
 
 ;; Returns a function which selects the text from 
 ;; the nodes selected by the given sxpath.
 (define-syntax select-single-node-text
   (syntax-rules ()
     ((_ path-exp ns)
      (let ((sxp (sxpath path-exp ns)))
	(lambda (nodelst)
	  (sxml:text (sxp nodelst)))))))

)
