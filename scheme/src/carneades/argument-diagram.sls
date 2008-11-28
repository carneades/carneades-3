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
 
 (import (rnrs base)
         (rnrs files)
         ; (rnrs hashtables)
         (rnrs io simple)
         (carneades lib srfi format)
         (carneades statement)
         (carneades argument)
         (carneades stream)
         (prefix (carneades table) table:)
         (prefix (carneades unify) unify:)
         (prefix (carneades config) config:)
         (carneades system)) 
 
 (define ids (table:make-table))
 
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
 
 ; diagram*: argument-graph context substitutions
 ;           (-> statement string) output-port -> void
 (define (diagram* ag context subs statement->string port)
   (set! ids (table:make-table)) ; re-initialize
   (format port "digraph g {~%    rankdir = \"RL\";~%")
   (print-statements ag context subs (statements ag) statement->string  port)
   (print-arguments ag context subs (list-arguments ag) port)
   (format port "}~%"))
 
 
 ; print-statements: argument-graph context substitutions 
 ;               (list-of statement) (-> statement string) output-port -> void
 (define (print-statements ag c subs statements statement->string port)
   (define (print-statement n)
     (let ((id (get-id n)))
       (format port "    ~A [shape=box, label=~S, style=~S];~%"
                id
                (statement->string (subs n))
                (if (or (and (acceptable? ag c n)
                             (not (rejected? c n)))
                        (accepted? c n))
                    "filled" 
                    ""))))
   (for-each print-statement statements))
 
 ; print-arguments: argument-graph context substitutions
 ;                  (list-of argument) output-port -> void
 (define (print-arguments ag c subs args port)
   (define (print-argument arg)
     (format port "    ~A [shape=ellipse, label=~S, style=~S];~%"
              (get-id (argument-id arg))
              (if (and (argument-scheme arg)
                       (< 0 (string-length (argument-scheme arg))))
                  (argument-scheme arg)
                  (symbol->string (argument-id arg)))
              (if (all-premises-hold? ag c arg) "filled" ""))
     (format port "    ~A -> ~A~A;~%" 
              (get-id (argument-id arg))
              (get-id (argument-conclusion arg))
              (if (eq? (argument-direction arg) 'con) " [arrowhead=\"onormal\"]" ""))
     (for-each (lambda (p) 
                 (format port "    ~A -> ~A [arrowhead=\"~A~A\"];~%" 
                          (get-id (premise-atom p))
                          (get-id (argument-id arg))
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
          (tmp-dot (string-append config:tmpdir
                                  (symbol->string (gensym "carneades-")) ".dot")))
     (call-with-output-file tmp-dot
       (lambda (port) (diagram* ag c subs statement->string port)))
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
 
 
 ; diagram: argument-graph context -> void
 (define (diagram ag c)
   (diagram* ag c unify:identity (lambda (s) (format "~a" s)) (current-output-port)))
 
 ; view: argument-graph context -> void
 (define (view ag c)
   (view* ag c unify:identity (lambda (s) (statement-formatted s))))
 
 ) ; end library
