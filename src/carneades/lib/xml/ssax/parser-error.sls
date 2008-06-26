(library
 (carneades lib  xml ssax parser-error)
 
 (export parser-error ssax:warn)
 
 (import 
  (rnrs base)
  (only (rnrs io ports)
	port?)
  (only (carneades lib  xml sxml env)
	cerr nl))
 
 (define parser-error
   (lambda  args
     (if (port? (car args))
	(cerr nl "Error at position " 
	      ;; (file-position (car args)) nl
	      (cdr args))
	(cerr nl "Error in error handler: its first parameter is not a port" 
	      nl args))
     (cerr nl)
     ;; (exit -1)  ; this exit makes me completely insane!
     (error 'parser-error args)))
 
 (define ssax:warn
   (lambda  args
     (if (port? (car args))
	(cerr nl "Warning at position " 
	      ;; (file-position (car args)) nl
	      (cdr args) nl)
	#f)))
  
)

