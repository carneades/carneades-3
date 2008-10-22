; PS/Tk Example Program "Hello World"
; Copyright (C) 2006 Nils M Holm
; See the PS/Tk license for conditions of use.

#!r6rs 
(import (rnrs)
        (carneades lib pstk main))

(define tk (tk-start))
(let* ((label (tk 'create-widget 'label
                  'width: 20
                  'height: 5
                  'text: "Hello, World!"
                  'font: "Helvetica 20"
                  'fg: "#ff0000"))
       (bt-quit (tk 'create-widget 'button
                    'text: "Goodbye"
                    'command: (lambda () (tk-end)))))
  (tk/pack label bt-quit)
  (tk-event-loop tk))
