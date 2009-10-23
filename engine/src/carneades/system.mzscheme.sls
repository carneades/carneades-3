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
 (carneades system)
 
 (export system pretty-print (rename (textual-tcp-connect tcp-connect)) gensym run-program)
 
 (import (rnrs)
         (only (scheme base) system-type subprocess)
         (only (scheme system) system) 
         (only (scheme pretty) pretty-print)
         (only (scheme tcp) tcp-connect)
         )
 
 (define (textual-tcp-connect hostname port-number)
   (call-with-values (lambda () (tcp-connect hostname port-number))
                     (lambda (byte-in byte-out)
                       (values (transcoded-port byte-in (make-transcoder (utf-8-codec)))
                               (transcoded-port byte-out (make-transcoder (utf-8-codec)))))))
    
 (define gensym-counter 0)
 
 (define gensym
   (case-lambda 
     (() 
      (set! gensym-counter (+ 1 gensym-counter))
      (string->symbol (string-append "g" 
                                     (number->string gensym-counter))))
     ((prefix) ; symbol or string
      (set! gensym-counter (+ 1 gensym-counter))
      (string->symbol (string-append 
                       (if (symbol? prefix) 
                           (symbol->string prefix) 
                           prefix)
                       (number->string gensym-counter))))))
 
  (define (concatenate strings)
    (let loop ((result "")
               (rest strings))
      (cond ((null? rest) result)
            ((pair? rest) 
             (loop (if (string=? result "")
                       (car rest)
                       (string-append result " " (car rest)))
                   (cdr rest))))))
                 
  (define (run-program program . args)
   (let-values
       (((pid in out err)
         (if (eq? (system-type 'os) 'windows)
             (subprocess #f #f #f (concatenate (cons program args)) 'exact   ;  MZSCHEME / Windows
                     program)
             (subprocess #f #f #f "/bin/sh" "-c"   ;  MZSCHEME / Unix
                         (string-append "exec " (concatenate (cons program args)) " 2>&1")))))
     (let ((utf8-transcoder (make-transcoder (utf-8-codec))))
       (cons (transcoded-port in  utf8-transcoder)
             (transcoded-port out utf8-transcoder)))))

 
 )