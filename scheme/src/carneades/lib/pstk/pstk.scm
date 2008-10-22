#lang scheme

; PS/Tk -- A Portable Scheme Interface to the Tk GUI Toolkit
; Copyright (C) 2008 Kenneth A Dickey
; Copyright (C) 2006-2008 Nils M Holm
; Copyright (C) 2004 Wolf-Dieter Busch
; All rights reserved.
;
; Redistribution and use in source and binary forms, with or without
; modification, are permitted provided that the following conditions
; are met:
; 1. Redistributions of source code must retain the above copyright
;    notice, this list of conditions and the following disclaimer.
; 2. Redistributions in binary form must reproduce the above copyright
;    notice, this list of conditions and the following disclaimer in the
;    documentation and/or other materials provided with the distribution.
;
; THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
; FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
; OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
; OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
; SUCH DAMAGE.
;
; PS/Tk is based on Chicken/Tk by Wolf-Dieter Busch (2004):
; http://wolf-dieter-busch.de/html/Software/Tools/ChickenTk.htm
; which is in turn based on Scheme_wish by Sven Hartrumpf (1997, 1998):
; http://pi7.fernuni-hagen.de/hartrumpf/scheme_wish.scm
;
; These are the changes that I (Nils) made to turn Chicken/Tk into PS/Tk:
;
; - Removed all Chicken-isms except for PROCESS.
; - All PS/Tk function names begin with TK/ or TK-:
;     EVAL-WISH   --> TK-EVAL-WISH
;     GET-TK-VAR  --> TK-GET-VAR
;     SET-TK-VAR! --> TK-SET-VAR!
;     START-TK    --> TK-START
;     END-TK      --> TK-END
;     EVENT-LOOP  --> TK-EVENT-LOOP
; - Added TK-DISPATCH-EVENT.
; - Added TK-WAIT-FOR-WINDOW because TK/WAIT returned too early.
; - Removed some unused functions and variables.
; - Replaced keyword lists with property lists.
; - Removed ScrolledText compound widget.
; - Removed :WIDGET-NAME option.
; - Added a PLT Scheme version of RUN-PROGRAM.
;
; Contributions (in order of appearance):
; - Jens Axel Soegaard: PLT Scheme/Windows RUN-PROGRAM.
; - Taylor R Campbell: Scheme48 RUN-PROGRAM, portable GENSYM, and some R5RS
;   portability fixes.
; - Jeffrey T. Read: Gambit hacks (RUN-PROGRAM, keyword hack).
; - Marc Feeley: Various versions of RUN-PROGRAM (Bigloo, Gauche, Guile,
;   Kawa, Scsh, Stklos), SRFI-88 keyword auto-detection, some bug fixes.
; - David St-Hilaire: suggested catching unspecific value in form->string.
; - Ken Dickey: added Ikarus Scheme
; - Ken Dickey: added Larceny Scheme
; Thank you!
;
; Change Log:
; 2008-06-22 Added Larceny Scheme support.
; 2008-02-29 Added R6RS (Ikarus Scheme) support, added TTK/STYLE.
; 2007-06-27 Renamed source file to pstk.scm.
; 2007-06-27 Re-factored some large procedures, applied some cosmetics.
; 2007-06-26 FORM->STRING catches unspecific values now, so event handlers
;            no longer have to return specific values.
; 2007-06-26 Re-imported the following ports from the processio/v1 snowball:
;            Bigloo, Gauche, Guile, Kawa, Scsh, Stklos.
; 2007-06-26 Added auto-detection of SRFI-88 keywords.
; 2007-03-03 Removed callback mutex, because it blocked some redraw
;            operations. Use TK-WITH-LOCK to protect critical sections.
; 2007-02-03 Added Tile support: TTK-MAP-WIDGETS, TTK/AVAILABLE-THEMES,
;            TTK/SET-THEME.
; 2007-01-20 Added (Petite) Chez Scheme port.
; 2007-01-06 Fix: TK-WAIT-FOR-WINDOW requires nested callbacks.
; 2007-01-05 Added code to patch through fatal TCL messages.
; 2007-01-05 Protected call-backs by a mutex, so accidental double
;            clicks, etc cannot mess up program state.
; 2006-12-21 Made FORM->STRING accept '().
; 2006-12-18 Installing WM_DELETE_WINDOW handler in TK-START now, so it does
;            not get reset in TK-EVENT-LOOP.
; 2006-12-18 Made TK-START and TK-END return () instead of #<unspecific>
;            (which crashes FORM->STRING).
; 2006-12-12 Fixed some wrong Tcl quotation (introduced by myself).
; 2006-12-09 Added TK/BELL procedure.
; 2006-12-08 Replaced ATOM->STRING by FORM->STRING.
; 2006-12-06 Added TK-WAIT-UNTIL-VISIBLE.
; 2006-12-03 Made more variables local to outer LETREC.
; 2006-12-03 Added Gambit port and keywords hack.
; 2006-12-02 Added Scheme 48 port, portable GENSYM, R5RS fixes.
; 2006-12-02 Added PLT/Windows port.

(define *wish-program* "tclsh")
(define *wish-debug-input* #f)
(define *wish-debug-output* #f)

(define *use-keywords?*
  (or (not (symbol? 'text:))
      (not (symbol? ':text))
      (string=? "text" (symbol->string 'text:))
      (string=? "text" (symbol->string ':text))))

(define tk #f)
(define tk-dispatch-event #f)
(define tk-end #f)
(define tk-eval #f)
(define tk-event-loop #f)
(define tk-get-var #f)
(define tk-id->widget #f)
(define tk-set-var! #f)
(define tk-start #f)
(define tk-var #f)
(define tk-wait-for-window #f)
(define tk-wait-until-visible #f)
(define tk-with-lock #f)
(define tk/after #f)
(define tk/appname #f)
(define tk/bell #f)
(define tk/bgerror #f)
(define tk/bind #f)
(define tk/bindtags #f)
(define tk/caret #f)
(define tk/choose-color #f)
(define tk/choose-directory #f)
(define tk/clipboard #f)
(define tk/destroy #f)
(define tk/dialog #f)
(define tk/event #f)
(define tk/focus #f)
(define tk/focus-follows-mouse #f)
(define tk/focus-next #f)
(define tk/focus-prev #f)
(define tk/get-open-file #f)
(define tk/get-save-file #f)
(define tk/grab #f)
(define tk/grid #f)
(define tk/image #f)
(define tk/lower #f)
(define tk/message-box #f)
(define tk/option #f)
(define tk/pack #f)
(define tk/place #f)
(define tk/popup #f)
(define tk/raise #f)
(define tk/scaling #f)
(define tk/selection #f)
(define tk/update #f)
(define tk/useinputmethods #f)
(define tk/wait #f)
(define tk/windowingsystem #f)
(define tk/winfo #f)
(define tk/wm #f)
(define ttk-map-widgets #f)
(define ttk/available-themes #f)
(define ttk/set-theme #f)
(define ttk/style #f)

(letrec

  ((nl (string #\newline))

   (wish-input #f)

   (wish-output #f)

   (tk-is-running #f)

   (tk-ids+widgets '())

   (tk-widgets '())

   (commands-invoked-by-tk '())

   (inverse-commands-invoked-by-tk '())

   (in-callback #f)

   (callback-mutex #t)

   (ttk-widget-map '())

   (tk-init-string
     (apply string-append
            (apply append
                   (map (lambda (s)
                          (list s (string #\newline)))
'("package require Tk"
"if {[package version tile] != \"\"} {"
"    package require tile"
"}"
""
"namespace eval AutoName {"
"    variable c 0"
"    proc autoName {{result \\#\\#}} {"
"        variable c"
"        append result [incr c]"
"    }"
"    namespace export *"
"}"
""
"namespace import AutoName::*"
""
"proc callToScm {callKey args} {"
"    global scmVar"
"    set resultKey [autoName]"
"    puts \"(call $callKey \\\"$resultKey\\\" $args)\""
"    flush stdout"
"    vwait scmVar($resultKey)"
"    set result $scmVar($resultKey)"
"    unset scmVar($resultKey)"
"    set result"
"}"
""
"proc tclListToScmList {l} {"
"    switch [llength $l] {"
"        0 {"
"            return ()"
"        }"
"        1 {"
"            if {[string range $l 0 0] eq \"\\#\"} {"
"                return $l"
"            }"
"            if {[regexp {^[0-9]+$} $l]} {"
"                return $l"
"            }"
"            if {[regexp {^[.[:alpha:]][^ ,\\\"\\'\\[\\]\\\\;]*$} $l]} {"
"                return $l"
"            }"
"            set result \\\""
"            append result\\"
"                [string map [list \\\" \\\\\\\" \\\\ \\\\\\\\] $l]"
"            append result \\\""
""
"        }"
"        default {"
"            set result {}"
"            foreach el $l {"
"                append result \" \" [tclListToScmList $el]"
"            }"
"            set result [string range $result 1 end]"
"            return \"($result)\""
"        }"
"    }"
"}"
""
"proc evalCmdFromScm {cmd {properly 0}} {"
"    if {[catch {"
"        set result [uplevel \\#0 $cmd]"
"    } err]} {"
"        puts \"(error \\\"[string map [list \\\\ \\\\\\\\ \\\" \\\\\\\"] $err]\\\")\""
"    } elseif $properly {"
"        puts \"(return [tclListToScmList $result])\""
"    } else {"
"        puts \"(return \\\"[string map [list \\\\ \\\\\\\\ \\\" \\\\\\\"] $result]\\\")\""
"    }"
"    flush stdout"
"}")))))

   (report-error
     (lambda (x)
       (newline)
       (display x)
       (newline)
       (bottom x)))

; ----------------------------------------------------------------
; BEGINNING OF NON-PORTABLE SECTION
;
; This is where portability ends. You do need a procedure that
; runs the tcl shell as a subprocess with an input pipe and an
; output pipe attached. Without this procedure you are out of
; luck.
;
; RUN-PROGRAM returns a list containing two ports: (IN OUT).
; IN is used to receive responses from tclsh;
; OUT is used to send commands to tclsh;

; Comment this out:
;   (run-program
;     (lambda (program)
;       (report-error
;         "You need to choose a version of RUN-PROGRAM first.")))

; BIGLOO
;   (run-program
;     (lambda (program)
;       (let* ((proc (run-process
;                      "/bin/sh"
;                      "-c"
;                      (string-append "exec " program " 2>&1")
;                      :input :pipe
;                      :output :pipe))
;              (in (process-output-port proc))
;              (out (process-input-port proc)))
;         (list in out))))

; (PETITE) CHEZ
;   (run-program
;     (lambda (program)
;       (let* ((in/out/pid (process
;                            (string-append "/bin/sh -c \"exec "
;                                           program
;                                           " 2>&1\"")))
;              (in (car in/out/pid))
;              (out (cadr in/out/pid)))
;         (list in out))))

; CHICKEN
;   ; Prelude: (use posix)
;   (run-program
;     (lambda (program)
;       (let-values
;         (((in out pid)
;           (process (string-append "/bin/sh -c \"exec "
;                                   program
;                                   " 2>&1\""))))
;         (list in out))))
;
;   (flush-output-port flush-output)

; GAMBIT
;   (run-program
;     (lambda (program)
;       (let ((port
;               (open-process
;                 (list path: "/bin/sh"
;                 arguments: (list "-c" (string-append "exec " program))
;                 stderr-redirection: #t))))
;         (list port port))))
;
;   (flush-output-port force-output)

; GAUCHE
;   ; Prelude: (use gauche.process)
;   (run-program
;     (lambda (program)
;       (let* ((proc
;                (run-process
;                "/bin/sh" "-c" (string-append "exec " program " 2>&1")
;                :input :pipe
;                :output :pipe))
;              (in (process-output proc))
;              (out (process-input proc)))
;         (list in out))))
;
;   (flush-output-port flush)

; GUILE
;   (run-program
;     (lambda (program)
;       (letrec
;         ((open-i/o-process
;            (lambda (prog . args)
;              (let ((c2p (pipe))
;                    (p2c (pipe)))
;                (setvbuf (cdr c2p) _IONBF)
;                (setvbuf (cdr p2c) _IONBF)
;                (let ((pid (primitive-fork)))
;                  (cond ((= pid 0)
;                      (set-batch-mode?! #t)
;                      (let ((input-fdes (fileno (car p2c)))
;                            (output-fdes (fileno (cdr c2p))))
;                        (port-for-each
;                          (lambda (pt-entry)
;                            (false-if-exception
;                              (let ((pt-fileno (fileno pt-entry)))
;                                (if (not (or (= pt-fileno input-fdes)
;                                             (= pt-fileno output-fdes)))
;                                    (close-fdes pt-fileno))))))
;                        (cond ((not (= input-fdes 0))
;                            (if (= output-fdes 0)
;                                (set! output-fdes (dup->fdes 0)))
;                            (dup2 input-fdes 0)))
;                        (if (not (= output-fdes 1))
;                            (dup2 output-fdes 1))
;                        (apply execlp prog prog args)))
;                    (else
;                        (close-port (cdr c2p))
;                        (close-port (car p2c))
;                        (cons (car c2p)
;                              (cdr p2c)))))))))
;         (let* ((in/out
;                 (open-i/o-process "/bin/sh" "-c"
;                                   (string-append "exec " program)))
;                (in (car in/out))
;                (out (cdr in/out)))
;           (list in out)))))
;
;   (flush-output-port force-output)

; IKARUS
;   (run-program
;     (lambda (program)
;       (let-values (((pid from to errport) (process program)))
;         (let ((utf8-transcoder (make-transcoder (utf-8-codec))))
;         (list (transcoded-port to   utf8-transcoder)
;               (transcoded-port from utf8-transcoder))))))

; KAWA
;   (run-program
;     (lambda (program)
;       (let* ((proc (make-process
;                      program
;                      (gnu.expr.QuoteExp:nullExp:getValue)))
;              (in (make <gnu.mapping.InPort>
;                    (make <java.io.InputStreamReader>
;                      ((primitive-virtual-method
;                         <java.lang.Process> "getInputStream"
;                         <java.io.InputStream> ())
;                       proc)
;                      "8859_1")))
;              (out (make <gnu.mapping.OutPort>
;                     (make <java.io.OutputStreamWriter>
;                       (make <java.io.BufferedOutputStream>
;                         ((primitive-virtual-method
;                            <java.lang.Process> "getOutputStream"
;                            <java.io.OutputStream> ())
;                          proc))
;                       "8859_1"))))
;         (list in out))))
;   (flush-output-port force-output)

; LARCENY
;   ; Prelude: (requirement (require "Standard/unix"))
;   (run-program
;     (lambda (program)
;       (let* ((in/out/pid (process
;                            (string-append "/bin/sh -c \"exec "
;                                           program
;                                           " 2>&1\"")))
;               (in  (car  in/out/pid))
;               (out (cadr in/out/pid))
;               (utf8-transcoder (make-transcoder (utf-8-codec))))
;         (list (transcoded-port in  utf8-transcoder)
;               (transcoded-port out utf8-transcoder)))))

; MZSCHEME / Unix
   (run-program
      (lambda (program)
        (let-values
          (((pid in out err)
            (subprocess #f #f #f "/bin/sh" "-c"
              (string-append "exec " program " 2>&1"))))
          (list in out))))

   (flush-output-port flush-output)

; MZSCHEME / Windows
;   (run-program
;     (lambda (program)
;       (let-values
;         (((pid in out err)
;           (subprocess #f #f #f "c:/tcl/bin/tclsh.exe" 'exact
;                       "c:/tcl/bin/tclsh.exe")))
;         (list in out))))
;
;   (flush-output-port flush-output)

; Scheme 9 from Empty Space
;   ; Requires SYS Extension
;   (run-program
;      (lambda (program)
;        (sys:spawn (string-append "exec " program " 2>&1"))))
;
;   (flush-output-port sys:flush)

; SCHEME48
;   ; Prelude: ,open posix receiving signals i/o
;   (run-program
;     (lambda (program)
;       (receive (parent-in parent-out) (open-pipe)
;         (receive (child-in child-out) (open-pipe)
;           (cond ((fork)
;                  =>
;                  (lambda (child-pid)
;                    child-pid
;                    (close-input-port child-in)
;                    (close-output-port parent-out)
;                    (list parent-in child-out)))
;                 (else
;                  (remap-file-descriptors! child-in
;                                           parent-out
;                                           parent-out)
;                  (exec "/bin/sh" "-c" (string-append
;                                         "exec "
;                                         program))))))))
;   (flush-output-port force-output)

; SCSH
;   (run-program
;     (lambda (program)
;       (receive (parent-in parent-out) (pipe)
;         (receive (child-in child-out) (pipe)
;           (cond ((fork)
;                  =>
;                  (lambda (child-pid)
;                    child-pid
;                    (close-input-port child-in)
;                    (close-output-port parent-out)
;                    (list parent-in child-out)))
;                 (else
;                  (let ((in-fdes (port->fdes child-in))
;                        (out-fdes (port->fdes parent-out)))
;                    (dup in-fdes 0)
;                    (dup out-fdes 1)
;                    (dup out-fdes 2)
;                    (close-input-port child-in)
;                    (close-output-port child-out))
;                  (exec "/bin/sh" "-c" (string-append
;                                         "exec "
;                                         program))))))))
;
;   (flush-output-port force-output)

; STKLOS
;   (run-program
;     (lambda (program)
;       (let* ((proc (run-process
;                      "/bin/sh"
;                      "-c"
;                      (string-append "exec " program " 2>&1")
;                      :input :pipe
;                      :output :pipe))
;              (in (process-output proc))
;              (out (process-input proc)))
;         (list in out))))
;
; END OF NON-PORTABLE SECTION
; ----------------------------------------------------------------

   (flush-wish
     (lambda ()
       (flush-output-port wish-input)))

   (option?
     (lambda (x)
       (or (and *use-keywords?*
                (keyword? x))
           (and (symbol? x)
                (let* ((s (symbol->string x))
                       (n (string-length s)))
                  (char=? #\: (string-ref s (- n 1))))))))

   (make-option-string
     (lambda (x)
       (if (and *use-keywords?*
                (keyword? x))
           (string-append " -" (keyword->string x))
           (let ((s (symbol->string x)))
             (string-append " -" 
               (substring s 0 (- (string-length s) 1)))))))

   (improper-list->string
     (lambda (a first)
       (cond ((pair? a)
           (cons (string-append (if first "" " ")
                                (form->string (car a)))
                 (improper-list->string (cdr a) #f)))
         ((null? a) '())
         (else (list (string-append " . " (form->string a)))))))

   (form->string
     (lambda (x)
       (cond ((eq? #t x) "#t")
         ((eq? #f x) "#f")
         ((number? x) (number->string x))
         ((symbol? x) (symbol->string x))
         ((string? x) x)
         ((null? x) "()")
         ((pair? x)
           (string-append "("
             (apply string-append
                    (improper-list->string x #t))
             ")"))
         ((eof-object? x) "#<eof>")
         (else "#<other>"))))

   (string-translate
     (lambda (s map)
       (letrec
         ((s-prepend (lambda (s1 s2)
           (cond ((null? s1) s2)
             (else (s-prepend (cdr s1) (cons (car s1) s2))))))
          (s-xlate (lambda (s r)
            (cond ((null? s) (reverse r))
              (else (let ((n (assv (car s) map)))
                      (cond (n (s-xlate (cdr s)
                                 (s-prepend (string->list (cdr n)) r)))
                        (else (s-xlate (cdr s)
                                (cons (car s) r))))))))))
       (list->string
         (s-xlate (string->list s) '())))))

   (string-trim-left
     (lambda (str)
       (cond ((string=? str "") "")
             ((string=? (substring str 0 1) " ")
               (string-trim-left (substring str 1
                                  (string-length str))))
             (else str))))

   (get-property
     (lambda (key args . thunk)
       (cond ((null? args)
           (cond ((null? thunk) #f)
             (else ((car thunk)))))
         ((eq? key (car args))
           (cond ((pair? (cdr args)) (cadr args))
             (else (report-error (list 'get-property key args)))))
         ((or (not (pair? (cdr args)))
              (not (pair? (cddr args))))
           (report-error (list 'get-property key args)))
         (else (apply get-property key (cddr args) thunk)))))

   (tcl-true?
     (let ((false-values
             `(0 "0" 'false "false" ,(string->symbol "0"))))
        (lambda (obj) (not (memv obj false-values)))))

   (widget?
     (lambda (x)
       (and (memq x tk-widgets) #t)))

   (call-by-key
     (lambda (key resultvar . args)
       (cond ((and in-callback (pair? callback-mutex)) #f)
         (else (set! in-callback (cons #t in-callback))
               (let* ((cmd (get-property key commands-invoked-by-tk))
                      (result (apply cmd args))
                      (str (string-trim-left
                              (scheme-arglist->tk-argstring
                                (list result)))))
                 (set-var! resultvar str)
                 (set! in-callback (cdr in-callback))
                 result)))))

   (gen-symbol
     (let ((counter 0))
       (lambda ()
         (let ((sym (string-append "g" (number->string counter))))
           (set! counter (+ counter 1))
           (string->symbol sym)))))

   (widget-name
     (lambda (x)
       (let ((name (form->string x)))
         (cond ((member name ttk-widget-map)
             (string-append "ttk::" name))
           (else name)))))

   (make-widget-by-id
     (lambda (type id . options)
       (let
         ((result
            (lambda (command . args)
              (case command
                ((get-id) id)
                ((create-widget)
                  (let* ((widget-type (widget-name (car args)))
                         (id-prefix (if (string=? id ".") "" id))
                         (id-suffix (form->string (gen-symbol)))
                         (new-id (string-append id-prefix "." id-suffix))
                         (options (cdr args)))
                    (eval-wish
                      (string-append
                        widget-type
                        " "
                        new-id
                        (scheme-arglist->tk-argstring options)))
                    (apply make-widget-by-id
                           (append (list widget-type new-id)
                                   options))))
                ((configure)
                  (cond ((null? args)
                      (eval-wish
                        (string-append id " " (form->string command))))
                    ((null? (cdr args))
                      (eval-wish
                        (string-append
                        id
                        " "
                        (form->string command)
                        (scheme-arglist->tk-argstring args))))
                    (else
                      (eval-wish
                        (string-append
                          id
                          " "
                          (form->string command)
                          (scheme-arglist->tk-argstring args)))
                      (do ((args args (cddr args)))
                          ((null? args) '())
                        (let ((key (car args)) (val (cadr args)))
                          (cond ((null? options)
                              (set! options (list key val)))
                            ((not (memq key options))
                               (set! options
                                     (cons key (cons val options))))
                            (else (set-car! (cdr (memq key options))
                                            val))))))))
                ((cget)
                  (let ((key (car args)))
                    (get-property
                      key
                      options
                      (lambda ()
                        (eval-wish
                          (string-append
                            id
                            " cget"
                            (scheme-arglist->tk-argstring args)))))))
                ((call exec)
                  (eval-wish
                    (string-trim-left
                      (scheme-arglist->tk-argstring args))))
                (else
                  (eval-wish
                    (string-append
                      id
                      " "
                      (form->string command)
                      (scheme-arglist->tk-argstring args))))))))
         (set! tk-widgets (cons result tk-widgets))
         (set! tk-ids+widgets
               (cons (string->symbol id)
                     (cons result tk-ids+widgets)))
         result)))

   (scheme-arg->tk-arg
     (lambda (x)
       (cond ((eq? x #f) " 0")
             ((eq? x #t) " 1")
             ((eq? x '()) " {}")
             ((option? x) (make-option-string x))
             ((widget? x) (string-append " " (x 'get-id)))
             ((and (pair? x) (procedure? (car x)))
               (let* ((lambda-term (car x))
                      (rest (cdr x))
                      (l (memq lambda-term
                               inverse-commands-invoked-by-tk))
                      (keystr (if l (form->string (cadr l))
                                    (symbol->string (gen-symbol)))))
                 (if (not l)
                     (let ((key (string->symbol keystr)))
                       (set! inverse-commands-invoked-by-tk
                         (cons lambda-term
                               (cons key
                                     inverse-commands-invoked-by-tk)))
                       (set! commands-invoked-by-tk
                         (cons key
                               (cons lambda-term
                                     commands-invoked-by-tk)))))
                 (string-append " {callToScm "
                                keystr
                                (scheme-arglist->tk-argstring rest)
                                "}")))
             ((procedure? x)
               (scheme-arglist->tk-argstring `((,x))))
             ((list? x)
               (cond ((eq? (car x) '+)
                   (let ((result (string-trim-left
                                   (scheme-arglist->tk-argstring
                                     (cdr x)))))
                     (cond ((string=? result "") " +")
                       ((string=? "{" (substring result 0 1))
                         (string-append
                           " {+ "
                           (substring result 1
                             (string-length result))))
                       (else (string-append " +" result)))))
                 ((and (= (length x) 3)
                       (equal? (car x) (string->symbol "@"))
                       (number? (cadr x))
                       (number? (caddr x)))
                   (string-append
                     "@"
                     (number->string (cadr x))
                     ","
                     (number->string (caddr x))))
                 (else
                   (string-append
                     " {"
                     (string-trim-left
                       (scheme-arglist->tk-argstring x))
                     "}"))))
             ((pair? x)
               (string-append
                 " "
                 (form->string (car x))
                 "."
                 (form->string (cdr x))))
             ((string? x)
               (if (string->number x)
                   (string-append " " x)
                   (string-append
                     " \""
                     (string-translate x
                       '((#\\ . "\\\\") (#\" . "\\\"")
                         (#\[ . "\\u005b") (#\] . "\\]")
                         (#\$ . "\\u0024")
                         (#\{ . "\\{") (#\} . "\\}")))
                     "\"")))
             (else (string-append " " (form->string x))))))

   (scheme-arglist->tk-argstring
     (lambda (args)
       (apply string-append
              (map scheme-arg->tk-arg
                   args))))

   (make-wish-func
     (lambda (tkname)
       (let ((name (form->string tkname)))
         (lambda args
           (eval-wish
             (string-append
               name
               (scheme-arglist->tk-argstring args)))))))

   (read-wish
     (lambda ()
       (let ((term (read wish-output)))
         (cond (*wish-debug-output*
             (display "wish->scheme: ")
             (write term)
             (newline)))
         term)))

   (wish
     (lambda arguments
       (for-each
         (lambda (argument)
           (cond (*wish-debug-input*
               (display "scheme->wish: ")
               (display argument)
               (newline)))
           (display argument wish-input)
           (newline wish-input)
           (flush-wish))
         arguments)))

   (start-wish
     (lambda ()
       (let ((result (run-program *wish-program*)))
         (set! wish-input (cadr result))
         (set! wish-output (car result)))))

   (read-line
     (lambda (in)
       (letrec
         ((collect-chars
            (lambda (c s)
              (cond ((or (eof-object? c) (char=? c #\newline))
                  (apply string (reverse s)))
                (else (collect-chars (read-char in) (cons c s))))))
          (first-char
            (read-char in)))
         (cond ((eof-object? first-char) first-char)
           (else (collect-chars first-char '()))))))

   (eval-wish
     (lambda (cmd)
       (wish (string-append
               "evalCmdFromScm \""
               (string-translate cmd
                 '((#\\ . "\\\\") (#\" . "\\\"")))
               "\""))
       (let again ((result (read-wish)))
         (cond ((not (pair? result))
             (report-error (string-append
                      "An error occurred inside Tcl/Tk" nl
                      " --> " (form->string result)
                      " " (read-line wish-output))))
           ((eq? (car result) 'return)
             (cadr result))
           ((eq? (car result) 'call)
             (apply call-by-key (cdr result))
             (again (read-wish)))
           ((eq? (car result) 'error)
             (report-error (string-append
                      "An error occurred inside Tcl/Tk" nl
                      " " cmd nl
                      " --> " (cadr result))))
           (else (report-error result))))))

   (id->widget
     (lambda (id)
       (get-property
         (string->symbol (form->string id))
         tk-ids+widgets
         (lambda ()
           (if (tcl-true? (tk/winfo 'exists id))
             (make-widget-by-id
               (tk/winfo 'class id)
               (form->string id))
             #f)))))

   (var
     (lambda (varname)
       (set-var! varname "")
       (string-append
         "::scmVar("
         (form->string varname)
         ")")))

   (get-var
     (lambda (varname)
       (eval-wish
         (string-append
           "set ::scmVar("
           (form->string varname)
           ")"))))

   (set-var!
     (lambda (varname value)
       (eval-wish
         (string-append
           "set ::scmVar("
           (form->string varname)
           ") {"
           (form->string value)
           "}"))))

   (start
     (lambda ()
       (start-wish)
       (wish tk-init-string)
       (set! tk-ids+widgets '())
       (set! tk-widgets '())
       (set! in-callback #f)
       (set! tk (make-widget-by-id 'toplevel "." 'class: 'Wish))
       (set! commands-invoked-by-tk '())
       (set! inverse-commands-invoked-by-tk '())
       (tk/wm 'protocol tk 'WM_DELETE_WINDOW end-tk)))

   (end-tk
     (lambda ()
       (set! tk-is-running #f)
       (wish "after 200 exit")))

   (dispatch-event
     (lambda ()
       (let ((tk-statement (read-wish)))
         (if (and (list? tk-statement)
                  (eq? (car tk-statement) 'call))
             (apply call-by-key (cdr tk-statement))))))

   (loop
     (lambda ()
       (cond ((not tk-is-running)
           (if wish-output
               (tk/wm 'protocol tk 'WM_DELETE_WINDOW '())))
         (else (dispatch-event)
               (loop)))))

   (event-loop
     (lambda ()
       (set! tk-is-running #t)
       (loop)))

   (map-ttk-widgets
     (lambda (x)
       (cond ((eq? x 'all)
           (set! ttk-widget-map '("button" "checkbutton" "radiobutton"
                                  "menubutton" "label" "entry" "frame"
                                  "labelframe" "scrollbar" "notebook"
                                  "progressbar" "combobox" "separator"
                                  "scale" "sizegrip" "treeview")))
         ((eq? x 'none)
           (set! ttk-widget-map '()))
         ((pair? x) (set! ttk-widget-map
                          (map form->string x)))
         (else (report-error
                 (string-append
                   "Argument to TTK-MAP-WIDGETS must be "
                   "ALL, NONE or a list of widget types."))))))

   (string-split
     (lambda (c s)
       (letrec
         ((split (lambda (i k tmp res)
            (cond ((= i k)
                (if (null? tmp) res (cons tmp res)))
              ((char=? (string-ref s i) c)
                (split (+ i 1) k "" (cons tmp res)))
              (else (split (+ i 1) k
                      (string-append tmp
                        (string (string-ref s i)))
                      res))))))
         (reverse (split 0 (string-length s) "" '())))))

   (ttk-available-themes
     (lambda ()
       (string-split #\space (eval-wish "ttk::style theme names"))))

   (do-wait-for-window
     (lambda (w)
       (dispatch-event)
       (cond ((equal? (tk/winfo 'exists w) "0") '())
         (else (do-wait-for-window w)))))

   (wait-for-window
     (lambda (w)
       (let ((outer-allow callback-mutex))
         (set! callback-mutex #t)
         (do-wait-for-window w)
         (set! callback-mutex outer-allow))))

   (wait-until-visible
     (lambda (w)
       (tk/wait 'visibility w)))

   (lock!
     (lambda ()
       (set! callback-mutex
             (cons callback-mutex #t))))

   (unlock!
     (lambda ()
       (if (pair? callback-mutex)
           (set! callback-mutex
                 (cdr callback-mutex)))))

   (with-lock
     (lambda (thunk)
       (lock!)
       (thunk)
       (unlock!))))

  (set! tk-eval eval-wish)
  (set! tk-id->widget id->widget)
  (set! tk-var var)
  (set! tk-get-var get-var)
  (set! tk-set-var! set-var!)
  (set! tk-start start)
  (set! tk-end end-tk)
  (set! tk-dispatch-event dispatch-event)
  (set! tk-event-loop event-loop)
  (set! tk-wait-for-window wait-for-window)
  (set! tk-wait-until-visible wait-until-visible)
  (set! tk-with-lock with-lock)
  (set! tk/after (make-wish-func 'after))
  (set! tk/bell (make-wish-func 'bell))
  (set! tk/update (make-wish-func 'update))
  (set! tk/clipboard (make-wish-func 'clipboard))
  (set! tk/bgerror (make-wish-func 'bgerror))
  (set! tk/bind (make-wish-func 'bind))
  (set! tk/bindtags (make-wish-func 'bindtags))
  (set! tk/destroy (make-wish-func 'destroy))
  (set! tk/event (make-wish-func 'event))
  (set! tk/focus (make-wish-func 'focus))
  (set! tk/grab (make-wish-func 'grab))
  (set! tk/grid (make-wish-func 'grid))
  (set! tk/image (make-wish-func 'image))
  (set! tk/lower (make-wish-func 'lower))
  (set! tk/option (make-wish-func 'option))
  (set! tk/pack (make-wish-func 'pack))
  (set! tk/place (make-wish-func 'place))
  (set! tk/raise (make-wish-func 'raise))
  (set! tk/selection (make-wish-func 'selection))
  (set! tk/winfo (make-wish-func 'winfo))
  (set! tk/wm (make-wish-func 'wm))
  (set! tk/choose-color (make-wish-func "tk_chooseColor"))
  (set! tk/choose-directory (make-wish-func "tk_chooseDirectory"))
  (set! tk/dialog (make-wish-func "tk_dialog"))
  (set! tk/get-open-file (make-wish-func "tk_getOpenFile"))
  (set! tk/get-save-file (make-wish-func "tk_getSaveFile"))
  (set! tk/message-box (make-wish-func "tk_messageBox"))
  (set! tk/focus-follows-mouse (make-wish-func "tk_focusFollowsMouse"))
  (set! tk/focus-next (make-wish-func "tk_focusNext"))
  (set! tk/focus-prev (make-wish-func "tk_focusPrev"))
  (set! tk/popup (make-wish-func "tk_popup"))
  (set! tk/wait (lambda args (make-wish-func 'tkwait)))
  (set! tk/appname (make-wish-func "tk appname"))
  (set! tk/caret (make-wish-func "tk caret"))
  (set! tk/scaling (make-wish-func "tk scaling"))
  (set! tk/useinputmethods (make-wish-func "tk useinputmethods"))
  (set! tk/windowingsystem (make-wish-func "tk windowingsystem"))
  (set! ttk/available-themes ttk-available-themes)
  (set! ttk/set-theme (make-wish-func "ttk::style theme use"))
  (set! ttk/style (make-wish-func "ttk::style"))
  (set! ttk-map-widgets map-ttk-widgets))
