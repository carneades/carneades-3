;;; Carneades Argumentation Library and Tools.
;;; Copyright (C) 2008 Thomas F. Gordon, Fraunhofer FOKUS, Berlin
;;; 
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU Lesser General Public License version 3 (LGPL-3)
;;; as published by the Free Software Foundation.
;;; 
;;; This program is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for details.
;;; 
;;; You should have received a copy of the GNU Lesser General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

#!r6rs

(library 
 (carneades argument-diagram)
 
 (export diagram view diagram* view*)
 
 (import (except (rnrs base) assert)
         (rnrs files)
         (rnrs hashtables)
         (rnrs io simple)
         (carneades lib srfi format)
         (carneades statement)
         (carneades argument)
         (carneades stream)
         (prefix (carneades unify) unify:)
         (prefix (carneades config) config:)
         (only (carneades system) system path->string make-temporary-file)
         (carneades gensym))
 
 ; (require (lib "string.ss" "srfi" "13"))
 
 
 ; get-id: hash-table datum -> symbol
 ; get the symbol used to identify some datum, generating
 ; one if the datum does not yet have an identifier.
 ; The identifers are suitable for naming nodes and arrows 
 ; in the DOT language, for use with GraphViz
 (define (get-id tbl d)
   (hashtable-ref tbl 
                  d 
                  (lambda () 
                    (let ((v (gensym "g")))
                      (hashtable-set! tbl d v) v))))
 
 ; diagram*: argument-graph context substitutions
 ;           (-> statement string) output-port -> void
 (define (diagram* ag context subs statement->string port)
   (let ((tbl (make-eqv-hashtable)))
     (display "digraph g {\n    rankdir = \"RL\";\n" port)
     (print-statements ag context subs tbl (statements ag) statement->string  port)
     (print-arguments ag context subs tbl (list-arguments ag) port)
     (display "}\n" port)))
 
 
 ; print-statements: argument-graph context substitutions 
 ;              hash-table (list-of statement) (-> statement string) output-port -> void
 (define (print-statements ag c subs tbl statements statement->string port)
   (define (print-statement n)
     (let ((id (get-id tbl n)))
       (format port "    ~A [shape=box, label=~S, style=~S];\n"
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
     (format port "    ~A [shape=ellipse, label=~S, style=~S];\n"
              (get-id tbl (argument-id arg))
              (if (and (argument-scheme arg)
                       (< 0 (string-length (argument-scheme arg))))
                  (argument-scheme arg)
                  (symbol->string (argument-id arg)))
              (if (all-premises-hold? ag c arg) "filled" ""))
     (format port "    ~A -> ~A~A;\n" 
              (get-id tbl (argument-id arg))
              (get-id tbl (argument-conclusion arg))
              (if (eq? (argument-direction arg) 'con) " [arrowhead=\"onormal\"]" ""))
     (for-each (lambda (p) 
                 (format port "    ~A -> ~A [arrowhead=\"~A~A\"];\n" 
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
 
 ) ; end library
