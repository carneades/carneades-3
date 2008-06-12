(module argument-diagram mzscheme
  (require "statement.ss")
  (require "argument.ss")
  (require "stream.ss")
  (require (prefix unify: "unify.ss"))
  (require (prefix config: "config.ss"))
  (require (lib "string.ss" "srfi" "13"))
  (require (prefix stream: "stream.ss"))
  (require (lib "file.ss"))
  (require (lib "process.ss"))
  
  (provide diagram view diagram* view*)
  
  ; get-id: hash-table datum -> symbol
  ; get the symbol used to identify some datum, generating
  ; one if the datum does not yet have an identifier.
  ; The identifers are suitable for naming nodes and arrows 
  ; in the DOT language, for use with GraphViz
  (define (get-id tbl d)
    (hash-table-get tbl d 
                    (lambda () (let ((v (gensym "g")))(hash-table-put! tbl d v) v))))
  
  ; diagram*: argument-graph context substitutions
  ;           (-> statement string) output-port -> void
  (define (diagram* ag context subs statement->string port)
    (let ((tbl (make-hash-table 'equal)))
      (display "digraph g {\n    rankdir = \"RL\";\n" port)
      (print-statements ag context subs tbl (statements ag) statement->string  port)
      (print-arguments ag context subs tbl (list-arguments ag) port)
      (display "}\n" port)))
  
  
  ; print-statements: argument-graph context substitutions 
  ;              hash-table (list-of statement) (-> statement string) output-port -> void
  (define (print-statements ag c subs tbl statements statement->string port)
    (define (print-statement n)
      (let ((id (get-id tbl n)))
        (fprintf port "    ~a [shape=box, label=~v, style=~v];\n"
                 id
                 (statement->string (subs n))
                 (if (or (and (acceptable? ag c n)
                              (not (rejected? c n)))
                         (accepted? c n))
                     "filled" 
                     ""))))
    (for-each print-statement statements))
  
  ; print-arguments: argument-graph context substitutions
  ;                  hash-table (list-of argument) output-port -> void
  (define (print-arguments ag c subs tbl args port)
    (define (print-argument arg)
      (fprintf port "    ~a [shape=ellipse, label=~v, style=~v];\n"
               (get-id tbl (argument-id arg))
               (if (and (argument-scheme arg)
                        (< 0 (string-length (argument-scheme arg))))
                   (argument-scheme arg)
                   (symbol->string (argument-id arg)))
               (if (all-premises-hold? ag c arg) "filled" ""))
      (fprintf port "    ~a -> ~a~a;\n" 
               (get-id tbl (argument-id arg))
               (get-id tbl (argument-conclusion arg))
               (if (eq? (argument-direction arg) 'con) " [arrowhead=\"onormal\"]" ""))
      (for-each (lambda (p) 
                  (fprintf port "    ~a -> ~a [arrowhead=\"~a~a\"];\n" 
                           (get-id tbl (premise-atom p))
                           (get-id tbl (argument-id arg))
                           (cond ((assumption? p) "dot")
                                 ((exception? p) "odot") 
                                 (else "none"))
                           (if (negative-premise? p) "tee" "")))
                (argument-premises arg))) 
    (for-each print-argument args))
  
  ; view*: argument-graph context substitutions
  ;        (-> statement string) -> void
  ; Provides a convenient way to display an argument graph. 
  ; Based on code contributed by András Förhécz <fandrew@mit.bme.hu>.
  ; To do: find a way to put the viewer process in the background, but still have the 
  ; temporary files deleted after use.
  
  (define (view* ag c subs statement->string)
    (let* ((suffix config:preferred-graphic-format)
           (format (if (equal? config:preferred-graphic-format "ps") 
                       "ps2" 
                       config:preferred-graphic-format))
           (tmp-dot (make-temporary-file "mztmp~a.dot")))    
      (call-with-output-file tmp-dot
        (lambda (port) (diagram* ag c subs statement->string port))
        'truncate)
      (if (equal? format "dot")
          (begin (system (string-append config:viewer " " (path->string tmp-dot)))
                 (delete-file tmp-dot))
          (let ((tmp-output (make-temporary-file (string-append "mztmp~a." suffix))))
            (system (string-append config:dot 
                                   (string-append " -T" format " ")
                                   (path->string tmp-dot) 
                                   " -o " 
                                   (path->string tmp-output)))
            (system (string-append config:viewer " " (path->string tmp-output)))
            (delete-file tmp-dot)
            (delete-file tmp-output)))))
  
  
  ; diagram: argument-graph context -> void
  (define (diagram ag c)
    (diagram* ag c unify:identity (lambda (s) (format "~a" s)) (current-output-port)))
  
  ; view: argument-graph context -> void
  (define (view ag c)
    (view* ag c unify:identity (lambda (s) (format "~a" s))))
  
  ) ; end module argument-map
