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

; To do: update this library to use the new argument-graph library 

(library 
 (carneades argument-diagram)
 
 (export diagram view diagram* view*)
 
 (import (rnrs base)
         (rnrs files)
         ; (rnrs hashtables)
         (rnrs io simple)
         (carneades base)
         (carneades lib srfi format)
         (carneades statement)
         (carneades argument)
         (carneades stream)
         (prefix (carneades table) table:)
         (prefix (carneades unify) unify:)
         (prefix (carneades config) config:)
         (carneades system)) 
 
 (define ids (table:make-table statement-hash statement=? null))
 
 ; get-id: datum -> symbol
 ; get the symbol used to identify some datum, generating
 ; one if the datum does not yet have an identifier.
 ; The identifers are suitable for naming nodes and arrows 
 ; in the DOT language, for use with GraphViz
 (define (get-id d)
   (let ((v1 (table:lookup ids d #f)))
     (or v1 
         (let ((v2 (gensym "g")))
           (set! ids (table:insert ids d v2))
           v2))))
 
 ; diagram*: argument-graph (-> statement string) output-port -> void
 (define (diagram* ag statement->string port)
   (set! ids (table:make-table statement-hash statement=? null)) ; re-initialize
   (format port "digraph g {~%    rankdir = \"RL\";~%")
   (print-statements ag (map node-statement (statement-nodes ag)) statement->string  port)
   (print-arguments ag (arguments ag) port)
   (format port "}~%"))
 
 
 ; print-statements: argument-graph (list-of statement) (-> statement string) output-port -> void
 (define (print-statements ag statements statement->string port)
   (define (print-statement n)
     (let ((id (get-id n)))
       (format port "    ~A [shape=box, label=~S, style=~S];~%"
                id
                (cond ((questioned? ag n)
                       (string-append "? " (statement->string n)))
                      ((accepted? ag n)
                       (string-append "+ " (statement->string n)))
                      ((rejected? ag n)
                       (string-append "- " (statement->string n)))
                      (else (statement->string n)))
                (cond ((and (acceptable? ag n)
                            (acceptable? ag (statement-complement n)))
                       "dotted,filled")
                      ((acceptable? ag n) "filled")
                      ((acceptable? ag (statement-complement n)) "dashed,filled")
                      (else "solid")))))
   (for-each print-statement statements))
 
 ; print-arguments: argument-graph (list-of argument) output-port -> void
 (define (print-arguments ag args port)
   (define (print-argument arg)
     (format port "    ~A [shape=ellipse, label=~S, style=~S];~%"
             (get-id (argument-id arg))
             (if (and (argument-scheme arg)
                      (< 0 (string-length (argument-scheme arg))))
                 (argument-scheme arg)
                 (symbol->string (argument-id arg)))
             (if (applicable? ag arg) "filled" "none"))
     (format port "    ~A -> ~A [arrowhead=~S];~%" 
             (get-id (argument-id arg))
             (get-id (argument-conclusion arg))
             (case (argument-direction arg)
               ((pro) "normal")
               ((con) "onormal")))
     (for-each (lambda (p) 
                 (format port "    ~A -> ~A [style=~S, arrowhead=~S];~%" 
                         (get-id (premise-atom p))
                         (get-id (argument-id arg))
                         (cond ((assumption? p) "dotted")
                               ((exception? p) "dashed") 
                               (else "solid")) ; ordinary premise
                         (if (negative-premise? p) "tee" "none")))
               (argument-premises arg))) 
   (for-each print-argument args))
 
 ; view*: argument-graph (-> statement string) -> void
 ; Provides a convenient way to display an argument graph. 
 ; Based on code contributed by András Förhécz <fandrew@mit.bme.hu>.
 ; To do: find a way to put the viewer process in the background, but still have the 
 ; temporary files deleted after use.
 
 (define (view* ag statement->string)
   (let* ((suffix config:preferred-graphic-format)
          (format (if (equal? config:preferred-graphic-format "ps") 
                      "ps2" 
                      config:preferred-graphic-format))
          (tmp-dot (string-append config:tmpdir
                                  (symbol->string (gensym "carneades-")) ".dot")))
     (call-with-output-file tmp-dot
       (lambda (port) (diagram* ag statement->string port)))
     (if (equal? format "dot")
         (begin (system (string-append config:viewer " " tmp-dot))
                (delete-file tmp-dot))
         (let ((tmp-output (string-append config:tmpdir
                                          (symbol->string (gensym "carneades-")) "." suffix)))

           (system (string-append 
                    config:dot 
                   " -T " 
                   format
                   " "
                   tmp-dot
                   " -o " 
                   tmp-output))
           (system (string-append config:viewer " " tmp-output))
           (delete-file tmp-dot)
           (delete-file tmp-output)))))
 
 
 ; diagram: argument-graph -> void
 (define (diagram ag)
   (diagram* ag (lambda (s) (format "~a" s)) (current-output-port)))
 
 ; view: argument-graph  -> void
 (define (view ag)
   (view* ag (lambda (s) (statement-formatted s))))
 
 ) ; end library
