#!r6rs

(import (rnrs)
        (carneades base)
        (carneades lib pstk)
        (carneades statement)
        (carneades argument)
        (carneades lkif2)
        (carneades argument-diagram)
        (carneades argument-search)
        (carneades argument-builtins)
        (carneades stream)
        (carneades lib srfi format)
        (prefix (carneades search) search:)
        (carneades rule)
        (prefix (carneades table) table:))


(define tk (tk-start))
(tk/wm 'withdraw tk)
(tk/appname "Carneades")
(tk/wm 'title tk "Carneades")


; document: represent an LKIF document
(define-record-type document 
   (fields filename  ; a string representing the full path to the LKIF file
           (mutable data)))    ; an lkif-data object from the lkif2 library

(define *documents* null) ; a list of open documents
(define *current-document* #f)
(define *current-argument-graph* #f)
(define *search-rest* #f) ; stream of remaining states found by argument search

; *statements-table*:  maps strings to statements
; assumption: no two statements have the same formatted string representation 
(define *statements-table* (make-hashtable string-hash string=?))

;(define *current-issue* (tk-var 'current-issue))
;(define *current-solution* (tk-var 'current-solution))

;; utilities

; replace-element: list datum datum -> list
; replaces e1 by e2 in the list l. e1 and e2 are compared using eq?
; If e1 is not in l, l is returned unchanged.
(define (replace-element l e1 e2)
  (cond ((null? l) null)
        ((eq? e1 (car l)) 
         (cons e2 (cdr l)))
        (else (cons (car l)
                    (replace-element (cdr l) e1 e2)))))
      
      

; find-argument-graph: document symbol -> argument-graph | #f
(define (find-argument-graph doc arg-graph-id)
  (let* ((data (document-data doc))
         (ags (lkif-data-argument-graphs data)))
    (find (lambda (ag) 
            (eq? arg-graph-id (argument-graph-id ag)))
          ags)))


; current-issue: void -> statement | #f
(define current-issue
  (lambda () 
    (if *current-argument-graph* 
        (argument-graph-main-issue *current-argument-graph*)
        #f)))
          
; commands

(define open-lkif
  (lambda ()
    (let ((filename (tk/get-open-file)))
      (call/cc (lambda (escape)
                 (with-exception-handler
                  ; fail if any exception is raised
                  (lambda (exn) 
                    (escape (tk/message-box 'icon: 'warning
                                            'message: "Unable to open or load the selected file."
                                            'type: 'ok
                                            'parent: tk)))
                  (lambda ()
                    (set! *documents* (cons (make-document filename (lkif-import filename))
                                            *documents*))
                    (set! *current-document* (car *documents*))
                    ))))
      (if *current-document* 
          (let ((ag (load-document! *current-document*)))
            (if ag (begin (set! *current-argument-graph* ag)
                          (load-argument-graph! ag))))))))

(define save-lkif
  (lambda ()
    (let ((filename (tk/get-save-file)))
      (call/cc (lambda (escape)
                 (with-exception-handler
                  ; fail if any exception is raised
                  (lambda (exn) 
                    (escape (tk/message-box 'icon: 'warning
                                            'message: "Unable to save to the selected file."
                                            'type: 'ok
                                            'parent: tk)))
                  (lambda ()
                    (if *current-document*
                          (lkif-export (document-data *current-document*) filename)))) )))))


(define show-argument-map
  (lambda ()
    (if *current-argument-graph*
          (view* *current-argument-graph* statement-formatted)
        (tk/message-box 'icon: 'warning
                        'message: "No argument graph to map."
                        'type: 'ok
                        'parent: tk))))


; load-document!: document -> argument-graph | #f
(define (load-document! document)
  (let* ((data (document-data document))
         (rules (lkif-data-rulebase data))
         (argument-graphs (lkif-data-argument-graphs data)))
    
    (clear-panels!)

    ; load the rules
    (if (not (null? (rulebase-rules rules)))
        (begin
          (for-each (lambda (rule) 
                      (rule-list 'insert ""
                                 'end
                                 'values: (list (rule-id rule))))
                    (rulebase-rules rules))
          ; display the first rule
          (load-rule! (car (rulebase-rules rules)))))
    
    ; load the argument graphs
    (if (not (null? argument-graphs))
        (begin (for-each (lambda (ag)
                           (argument-graph-table 'insert ""
                                                 'end 
                                                 'values: (list (symbol->string (argument-graph-id ag))
                                                                (argument-graph-title ag))))
                         argument-graphs)
               ; return the first argument graph
               (car argument-graphs))
        ; else return #f to indicate the document contains no argument graphs
        #f))) 

(define (format-clause clause)
  (let ((n (length clause)))
    (cond ((= n 0) "")
          ((= n 1) (statement-formatted (car clause)))
          ((> n 1) (string-append (statement-formatted (car clause))
                                  "; " 
                                  (format-clause (cdr clause)))))))

; load-rule!: rule -> void
(define (load-rule! rule)
  (rule-id-field 'delete 0 'end)
  (rule-id-field 'insert 0 (rule-id rule))
  (rule-head-list 'delete (rule-head-list 'children ""))
  (for-each (lambda (head)
              (rule-head-list 'insert ""
                  'end
                  'values: (list (statement-formatted head))))
            (rule-head rule))
  (rule-body-list 'delete (rule-body-list 'children ""))
  (for-each (lambda (clause)
              (rule-body-list 'insert ""
                  'end
                  'values: (list (format-clause clause))))
            (rule-body rule)))

; load-argument-graph!: argument-graph -> void
(define (load-argument-graph! ag)
  (let* ((data (document-data *current-document*))
         (issue (argument-graph-main-issue ag))
         (issue-formatted (statement-formatted issue)))
    ; load the query form
    (hashtable-set! *statements-table* issue-formatted issue)
    (tk-set-var! 'current-issue issue-formatted)
    ; load the statements table
    (for-each (lambda (node)
                (let* ((p (node-statement node))
                       (id "") 
                       (s (status ag p))
                       (a (let ((ap (acceptable? ag p))
                                (an (acceptable? ag (statement-complement p))))
                            (cond ((and ap an)
                                   "P,¬P")
                                  (ap "P")
                                  (an "¬P")
                                  (else ""))))
                       (pro (length (pro-arguments ag p)))
                       (con (length (con-arguments ag p)))
                       (ps (proof-standard ag p))
                       (content (statement-formatted p))
                       (row (list id a s pro con ps content)))
                  (hashtable-set! *statements-table* content p)
                  (statement-table 'insert ""
                                   'end
                                   'values: row)))
              (list-nodes ag))))


(define (clear-panels!)
  ; clear fact panel
  (fact-table 'delete (fact-table 'children ""))
  ; clear rule panel  
  (rule-list 'delete (rule-list 'children ""))
  (rule-id-field 'delete 0 'end)
  (rule-head-list 'delete (rule-head-list 'children ""))
  (rule-body-list 'delete (rule-body-list 'children ""))
  ; clear argument
  (argument-graph-table 'delete (argument-graph-table 'children ""))
  (clear-argument-graph!))

(define (clear-argument-graph!)
  (statement-table 'delete (statement-table 'children ""))
  (argument-tree 'delete (argument-tree 'children ""))
  (premise-table 'delete (premise-table 'children "")))


; update-argument-graphs!: document symbol argument-graph -> void
; If there is an argument graph with the 
; given id, replace the argument-graph. If there is no such argument graph, add the new
; argument graph to the list of argument graphs of the document.
(define (update-argument-graphs! doc arg-graph-id new-ag)
  (let* ((old-ag (find-argument-graph doc arg-graph-id))
         (d (document-data doc))
         (ags (lkif-data-argument-graphs d)))
    (document-data-set! doc 
                        (make-lkif-data (lkif-data-sources d)
                                        (lkif-data-rulebase d)
                                        (if old-ag 
                                            (replace-element ags old-ag new-ag)
                                            (append ags (list new-ag)))))))

(define insert-argument-graph!
  (lambda ()
    ; (printf "debug: insert-argument-graph begin~%")
    (if *current-argument-graph* 
        (let* ((frame (tk 'create-widget 'toplevel 
                          'class: 'ttk::frame
                          'bg: 'gray90))
               (lw (string-length "Title:")) ; label-width
               (ew 40) ; entry-width
               (id-frame (frame 'create-widget 'ttk::frame))
               (id-label (id-frame 'create-widget 'ttk::label 
                                   'text: "ID:"
                                   'width: lw))
               (id-entry (id-frame 'create-widget 'ttk::entry
                                   'width: ew))
               (title-frame (frame 'create-widget 'ttk::frame))
               (title-label (title-frame 'create-widget 'ttk::label
                                         'text: "Title:"
                                         'width: lw))
               (title-entry (title-frame 'create-widget 'ttk::entry
                                         'width: ew))
               (issue-frame (frame 'create-widget 'ttk::frame))
               (issue-label (issue-frame 'create-widget 'ttk::label
                                         'text: "Issue:"
                                         'width: lw))
               (issue-entry (issue-frame 'create-widget 'ttk::entry
                                         'width: ew))
               (buttons-frame (frame 'create-widget 'ttk::frame))
               (cancel-button (buttons-frame 'create-widget 'ttk::button
                                             'text: "Cancel"
                                             'command: (lambda () (tk/wm 'withdraw frame))))
               (ok-cmd (lambda ()
                         ; to do:  validate the id and title entries
                         (let* ((id (string->symbol (id-entry 'get)))
                                (title (title-entry 'get))
                                (issue (read (open-string-input-port (issue-entry 'get))))
                                (ag (make-argument-graph id
                                                         title
                                                         issue)))
                           (tk/wm 'withdraw frame)
                           (update-argument-graphs! *current-document*
                                                    id
                                                    ag)
                           (load-document! *current-document*) ; to refresh the list of argument graphs
                           (load-argument-graph! ag))))
               
               (ok-button (buttons-frame 'create-widget 'ttk::button
                                         'text: "OK"
                                         'command: ok-cmd)) 
               
               (px "2m")
               (py "2m"))
          
          
          (tk/wm 'title frame "Insert New Argument Graph")
          (tk/wm 'resizable frame #f #f)
          ; (id-entry 'insert 0 (argument-graph-id ag))
          ; (title-entry 'insert 0 (argument-graph-title ag))
          (issue-entry 'insert 0 "(predicate subject object)")
          
          (tk/pack id-label id-entry 'side: 'left)
          (tk/pack title-label title-entry 'side: 'left)
          (tk/pack issue-label issue-entry 'side: 'left)
          (tk/pack cancel-button ok-button 'side: 'left)
          (tk/grid id-frame 'row: 0 'column: 0 'columnspan: 2 'sticky: 'w 'padx: px 'pady: py)   
          (tk/grid title-frame 'row: 1 'column: 0 'columnspan: 2 'sticky: 'w 'padx: px 'pady: py)
          (tk/grid issue-frame 'row: 2 'column: 0 'columnspan: 2 'sticky: 'w 'padx: px 'pady: py)
          (tk/grid buttons-frame 'row: 3 'column: 1 'padx: px 'pady: py) ))))
  

; menu bar

(define menubar (tk 'create-widget 'menu))
(tk 'configure 'menu: menubar)

; to do: find a way to remove the "tclsh" menu from the menubar.

; Carneades menu

(define carneades-menu (menubar 'create-widget 'menu))
(menubar 'add 'cascade 'label: "Carneades" 'menu: carneades-menu)

(carneades-menu 'add 'command 
                'label: "About Carneades" 
                'command: (lambda ()   (tk/message-box 'icon: 'info
                  'message: 
"
Carneades Argumentation System
Copyright © 2008 Thomas F. Gordon
Fraunhofer FOKUS, Berlin
http://carneades.berlios.de
"
                  'type: 'ok
                  'parent: tk)))

(carneades-menu 'add 'separator)
(carneades-menu 'add 'command 
                'label: "Quit" 
                'accelerator: "Command+Q" 
                'command: tk-end)


; file menu
(define file-menu (menubar 'create-widget 'menu))
(menubar 'add 'cascade 
         'label: "File"
         'menu: file-menu)

(file-menu 'add 'command 
           'label: "Open" 
           'accelerator: "Command+O"
           'command: open-lkif )


(file-menu 'add 'command 
           'label: "Save As	" 
           'accelerator: "Command+S"
           'command: save-lkif )


;; view menu
;
;(define view-menu (menubar 'create-widget 'menu))
;(menubar 'add 'cascade 
;         'label: "View"
;         'menu: view-menu)
;(view-menu 'add 'command 
;           'label: "Argument Map" 
;           'command: show-argument-map)

; edit menu

(define edit-menu (menubar 'create-widget 'menu))
(menubar 'add 'cascade
         'label: "Edit"
         'menu: edit-menu)
(edit-menu 'add 'command
             'label: "Undo"
             'command: (lambda () 'todo))
(edit-menu 'add 'command
             'label: "Redo"
             'command: (lambda () 'todo))
(edit-menu 'add 'separator)
(edit-menu 'add 'command
             'label: "Delete"
             'command: (lambda () 'todo))
(edit-menu 'add 'command
             'label: "Inspect"
             'command: (lambda () 'todo))

; insert menu

(define insert-menu (menubar 'create-widget 'menu))
(menubar 'add 'cascade
         'label: "Insert"
         'menu: insert-menu)
(insert-menu 'add 'command
             'label: "Argument Graph"
             'command: insert-argument-graph!)

; help menu
(define help-menu (menubar 'create-widget 'menu))
(menubar 'add 'cascade 
         'label: "Help" 
         'menu: help-menu)
(help-menu 'add 'command 
           'label: "Carneades Help" 
           'command: (lambda () (tk/message-box 'icon: 'warning
                                                'message: "Sorry."
                                                'type: 'ok
                                                'parent: tk)))

; command key equivalents

(tk/bind tk "<Command-q>" tk-end)
(tk/bind tk "<Command-o>" open-lkif)
(tk/bind tk "<Command-s>" save-lkif)

; tabs and panels of the main window

(define nb1 (tk 'create-widget 'ttk::notebook))
(tk/pack nb1 'fill: 'both 'expand: 1 'padx: 2 'pady: 3)

;; fact table

(define fact-table (nb1 'create-widget 'ttk::treeview
                       'columns: '(id value statement)
                       'show: '(headings)))
(fact-table 'heading 'id 'text: "ID")
(fact-table 'heading 'value 'text: "Truth Value")
(fact-table 'heading 'statement 'text: "Statement")

(fact-table 'column 'id 'width: 100 'minwidth: 100 'stretch: 0)
(fact-table 'column 'value 'width: 80 'minwidth: 80 'stretch: 0)
(fact-table 'column 'statement 'width: 500 'minwidth: 500)



;; rule panel: list of rules and a rule inspector

(define rule-panel (nb1 'create-widget 'ttk::frame 'borderwidth: 2))

;; rule list

(define rule-list-frame 
    (rule-panel 'create-widget 'ttk::labelframe
                        'text: "Rules"
                        'padding: 2))

(define rule-list (rule-list-frame 'create-widget 'ttk::treeview
                                   'columns: '(id)
                                   'height: 33
                                   'show: '(headings)))
(rule-list 'heading 'id 'text: "ID")

;; rule inspector

(define rule-column-width 850)

(define rule-id-frame (rule-panel 'create-widget 'ttk::labelframe 
                                      'text: "ID"
                                      'borderwidth: 2))

(define rule-id-field (rule-id-frame 'create-widget 'ttk::entry
                                     'width: 106))

(define rule-head-frame (rule-panel 'create-widget 'ttk::labelframe 
                                        'text: "Head"
                                        'borderwidth: 2))

(define rule-head-list (rule-head-frame 'create-widget 'ttk::treeview
                                        'height: 5
                                        'columns: '(statement)
                                        'show: '(headings)))

(rule-head-list 'heading 'statement 'text: "Statement")
(rule-head-list 'column 'statement 'width: rule-column-width 'stretch: 1)

(define rule-body-frame (rule-panel 'create-widget 'ttk::labelframe 
                                    'text: "Body"
                                    'borderwidth: 2))

(define rule-body-list 
  (rule-body-frame 'create-widget 'ttk::treeview
                   'height: 19
                   'columns: '(clause)
                   'show: '(headings)))

(rule-body-list 'heading 'clause 'text: "Clause")
(rule-body-list 'column 'clause 'width: rule-column-width 'stretch: 1)

(tk/grid rule-list-frame 'row: 0 'column: 0 'rowspan: 3 'padx: "2m" 'pady: "2m")
(tk/grid rule-list 'row: 0 'column: 0 'padx: "2m" 'pady: "2m")
(tk/grid rule-id-frame 'row: 0 'column: 1 'sticky: 'we 'padx: "2m" 'pady: "2m")
(tk/grid rule-id-field 'row: 0 'column: 0 'padx: "2m" 'pady: "2m")
(tk/grid rule-head-frame 'row: 1 'column: 1 'sticky: 'we 'padx: "2m" 'pady: "2m")
(tk/grid rule-head-list 'row: 0 'column: 0 'sticky: 'we 'padx: "2m" 'pady: "2m")
(tk/grid rule-body-frame 'row: 2 'column: 1 'sticky: 'we 'padx: "2m" 'pady: "2m")
(tk/grid rule-body-list 'row: 0 'column: 0 'sticky: 'we 'padx: "2m" 'pady: "2m")

(tk/bind rule-list "<<TreeviewSelect>>" 
         (lambda ()
           (if *current-document*
               (let* ((selection (rule-list 'selection))
                      (rule-id (string->symbol (rule-list 'item selection 'values:)))
                      (data (document-data *current-document*))
                      (rules (lkif-data-rulebase data))
                      (rule (get-rule rule-id rules)))
                 ; (printf "debug: rule id=~a; rule found=~a~%" rule-id (if rule #t #f))
                 (if rule (load-rule! rule))))))
             
; argument graph panel

(define argument-graph-panel 
   (nb1 'create-widget 'ttk::frame 'borderwidth: 2))

;; argument graph list

(define argument-graph-table-frame 
  (argument-graph-panel 'create-widget 'ttk::labelframe
                        'text: "Argument Graphs"
                        'padding: 2))
  
(define argument-graph-table
  (argument-graph-table-frame 'create-widget 'ttk::treeview
                        'columns: '(id title)
                        'height: 33
                        'show: '(headings)))
(argument-graph-table 'heading 'id 'text: "ID")
(argument-graph-table 'heading 'title 'text: "Title")
(argument-graph-table 'column 'id 'width: 50 'minwidth: 50 'stretch: 0)
(argument-graph-table 'column 'title 'width: 200 'minwidth: 200 'stretch: 0)

(tk/bind argument-graph-table "<<TreeviewSelect>>" 
         (lambda ()
           (let* ((s (argument-graph-table 'selection))
                  (ag-id (string->symbol (argument-graph-table 'set s 'id)))
                  (ag (find-argument-graph *current-document* ag-id)))
             (if ag 
                 (begin  
                   (set! *current-argument-graph* ag)
                   ; reset the search engine
                   (set! *search-rest* #f)
                   ; clear the statement argument and premise tables
                   (statement-table 'delete (statement-table 'children ""))
                   (argument-tree 'delete (argument-tree 'children ""))
                   (premise-table 'delete (premise-table 'children ""))
                
                   ; and then load the selected argument graph
                   (tk-set-var! 'current-solution "")
                   (load-argument-graph! ag))
                 (printf "debug: no argument graph~%")))))


; Save Argument Graph Dialog

; save-argument-graph-cmd: void -> void
; Command to save the argument-graph to the current-document.  The user
; is asked whether the argument graph with the given id 
; should be replaced by this graph, or whether it should be
; saved as a new graph, with a new id and title.
(define save-argument-graph-cmd
  (lambda () 
    (if *current-argument-graph* 
        (let* ((frame (tk 'create-widget 'toplevel 
                          'class: 'ttk::frame
                          'bg: 'gray90))
               (lw (string-length "Title:")) ; label-width
               (ew 40) ; entry-width
               (id-frame (frame 'create-widget 'ttk::frame))
               (id-label (id-frame 'create-widget 'ttk::label 
                                   'text: "ID:"
                                   'width: lw))
               (id-entry (id-frame 'create-widget 'ttk::entry
                                   'width: ew))
               (title-frame (frame 'create-widget 'ttk::frame))
               (title-label (title-frame 'create-widget 'ttk::label
                                         'text: "Title:"
                                         'width: lw))
               (title-entry (title-frame 'create-widget 'ttk::entry
                                   'width: ew))
               (buttons-frame (frame 'create-widget 'ttk::frame))
               (cancel-button (buttons-frame 'create-widget 'ttk::button
                                             'text: "Cancel"
                                             'command: (lambda () (tk/wm 'withdraw frame))))
              
               
               (ok-cmd (lambda ()
                         ; to do:  validate the id and title entries
                         (let* ((new-id (string->symbol (id-entry 'get)))
                                (ag2 (make-argument-graph new-id
                                                         (title-entry 'get)
                                                         (argument-graph-main-issue *current-argument-graph*)
                                                         (argument-graph-nodes *current-argument-graph*)
                                                         (argument-graph-arguments *current-argument-graph*))))
                           (tk/wm 'withdraw frame)
                           (update-argument-graphs! *current-document*
                                          new-id
                                          ag2)
                           (load-document! *current-document*)
                           (load-argument-graph! ag2))))

               (ok-button (buttons-frame 'create-widget 'ttk::button
                                         'text: "OK"
                                         'command: ok-cmd)) 
          
               (px "2m")
               (py "2m"))
          
 
          (tk/wm 'title frame "Save Argument Graph Dialog")
          (tk/wm 'resizable frame #f #f)
          (id-entry 'insert 0 (argument-graph-id *current-argument-graph*))
          (title-entry 'insert 0 (argument-graph-title *current-argument-graph*))
          
          (tk/pack id-label id-entry 'side: 'left)
          (tk/pack title-label title-entry 'side: 'left)
          (tk/pack cancel-button ok-button 'side: 'left)
          (tk/grid id-frame 'row: 0 'column: 0 'columnspan: 2 'sticky: 'w 'padx: px 'pady: py)   
          (tk/grid title-frame 'row: 1 'column: 0 'columnspan: 2 'sticky: 'w 'padx: px 'pady: py)  
          (tk/grid buttons-frame 'row: 2 'column: 1 'padx: px 'pady: py) ))))


; Search Dialog Panel

(define (search-dialog parent)
  (let* ((frame (parent 'create-widget 'ttk::labelframe
                        'text: "Search"
                        'padding: 2))
         (px "2m")
         (py "2m")
         (lw (string-length "Solution:"))  ; label width
         (limit-frame (frame 'create-widget 'ttk::frame))
         (limit-label (limit-frame 'create-widget 'ttk::label 
                                   'text: "Limit:"
                                   'anchor: 'w
                                   'width: lw))
         (limit-entry (limit-frame 'create-widget 'ttk::entry 
                                   'width: 5))
         (turns-frame (frame 'create-widget 'ttk::frame))
         (turns-label (turns-frame 'create-widget 'ttk::label 
                                   'text: "Turns:"
                                   'anchor: 'w
                                   'width: lw))
         (turns-entry (turns-frame 'create-widget 'ttk::entry 
                                   'width: 5))
         
         (issue-frame (frame 'create-widget 'ttk::frame))
         (issue-label (issue-frame 'create-widget 'ttk::label
                                   'text: "Issue:"
                                   'width: lw))
         (issue-entry (issue-frame 'create-widget 'ttk::entry
                                   'width: 90
                                   'textvariable: (tk-var 'current-issue)))
         
         (solution-frame (frame 'create-widget 'ttk::frame))
         (solution-label (solution-frame 'create-widget 'ttk::label
                                         'text: "Solution:"
                                         'width: lw))
         (solution-entry (solution-frame 'create-widget 'ttk::entry
                                         'width: 90
                                         'textvariable: (tk-var 'current-solution)))
         
         (search (lambda () 
                   (if (and *current-document* *current-argument-graph*)
                       (let* ((ag *current-argument-graph*)
                              (data (document-data *current-document*))
                              (issue (argument-graph-main-issue ag))
                              ; to do: validate entries in the form
                              (max-nodes (read (open-string-input-port (limit-entry 'get))))
                              (max-turns (read (open-string-input-port (turns-entry 'get))))
                              (rb (lkif-data-rulebase data))
                              (cqs '(excluded))  ; to do: check boxes
                              (generators (list builtins (generate-arguments-from-rules rb cqs)))
                              (side (if (in? ag issue) 'con 'pro))
                              (pro-goals (if (eq? side 'pro) 
                                             (list (list issue))
                                             null))
                              (con-goals (if (eq? side 'pro) 
                                             null 
                                             (list (list (statement-complement issue))))))
                         ; (printf "debug: issue=~w; side=~a~%" issue side)
                         (find-best-arguments search:depth-first 
                                              (search:make-resource max-nodes)
                                               max-turns
                                              (make-state issue side pro-goals con-goals ag)
                                              generators)))))
         (before-states null) ; list of states previously found, in reverse order
         (current-state #f)   ; the current state in the stream of search results
         (after-states null)  ; list of states after the current state
                          
         (next (lambda ()                 
                 (cond ((not *search-rest*)
                        (let ((str (search)))
                          (set! before-states null)
                          (if (not (stream-null? str))
                              (begin (set! current-state (stream-car str))
                                     (set! *search-rest* (stream-cdr str)))
                              (begin (set! current-state #f)
                                     (set! *search-rest* #f)
                                     (tk/message-box 'icon: 'warning
                                             'message: "No arguments found."
                                             'type: 'ok
                                             'parent: tk)))
                          (set! after-states null)))
                       
                       ((not (null? after-states))
                        (set! before-states (cons current-state before-states))
                        (set! current-state (car after-states))
                        (set! after-states (cdr after-states)))
                       
                       ((not (stream-null? *search-rest*))
                        (set! before-states (cons current-state before-states))
                        (set! current-state (stream-car *search-rest*))
                        (set! *search-rest* (stream-cdr *search-rest*)))
                       (else (tk/message-box 'icon: 'warning
                                             'message: "No further arguments found."
                                             'type: 'ok
                                             'parent: tk)))
                 (if current-state
                     (let* ((ag (state-arguments current-state))              
                            (subs (state-substitutions current-state))
                            (issue (argument-graph-main-issue ag)))
                       (clear-argument-graph!)
                       (set! *current-argument-graph* ag)
                       (tk-set-var! 'current-solution (statement-formatted (subs issue)))
                       (load-argument-graph! *current-argument-graph*)))))
                            
         (previous (lambda () 
                     (cond ((not *search-rest*) ; search not initialized, thus no previous solutions
                            (tk/message-box 'icon: 'warning
                                            'message: "No previous arguments."
                                            'type: 'ok
                                            'parent: tk))
                           ((not (null? before-states))
                            (set! after-states (cons current-state after-states))
                            (set! current-state (car before-states))
                            (set! before-states (cdr before-states))
                            (let* ((ag (state-arguments current-state))
                                   (subs (state-substitutions current-state))
                                   (issue (argument-graph-main-issue ag)))
                              (clear-argument-graph!)
                              (set! *current-argument-graph* ag)
                              (tk-set-var! 'current-solution (statement-formatted (subs issue)))
                              (load-argument-graph! *current-argument-graph*)))
                           (else (tk/message-box 'icon: 'warning
                                                 'message: "No previous arguments."
                                                 'type: 'ok
                                                 'parent: tk)))))

         (buttons-frame (frame 'create-widget 'ttk::frame))
  
         (next-button (buttons-frame 'create-widget 'ttk::button
                                     'text: "Next"
                                     'command: next))
         (previous-button (buttons-frame 'create-widget 'ttk::button
                                         'text: "Previous"
                                         'command: previous))
         
         (map-button (buttons-frame 'create-widget 'ttk::button
                                    'text: "Map"
                                    'command: show-argument-map))
         (save-button (buttons-frame 'create-widget 'ttk::button
                                     'text: "Insert"
                                     'command: save-argument-graph-cmd)))
    (limit-entry 'insert 0 "20")
    (turns-entry 'insert 0 "2")
    (tk/pack limit-label limit-entry 'side: 'left)
    (tk/pack turns-label turns-entry 'side: 'left)
    (tk/pack issue-label issue-entry 'side: 'left)
    (tk/pack solution-label solution-entry 'side: 'left)
    (tk/pack previous-button next-button map-button save-button  'side: 'left)
    (tk/grid limit-frame 'row: 0 'column: 0 'sticky: 'w 'padx: px 'pady: py)
    (tk/grid turns-frame 'row: 0 'column: 1 'sticky: 'w 'padx: px 'pady: py)
    (tk/grid buttons-frame 'row: 0 'column: 2 'padx: px 'pady: py)
    (tk/grid issue-frame 'row: 1 'column: 0 'columnspan: 3 'padx: px 'pady: py)
    (tk/grid solution-frame 'row: 2 'column: 0 'columnspan: 3 'padx: px 'pady: py)
    
    frame))

(define search-panel (search-dialog argument-graph-panel))



           
(define statement-table-frame
  (argument-graph-panel 'create-widget 'ttk::labelframe
                        'text: "Statements"
                        'padding: 2))

(define statement-table
  (statement-table-frame 'create-widget 'ttk::treeview
                         'columns: '(id acceptable status pro con standard content)
                         'displaycolumns: '(id acceptable status  pro con standard content)
                         'height: 7
                         'show: '(headings)))
(statement-table 'heading 'id 'text: "ID")
(statement-table 'heading 'status 'text: "Status")
(statement-table 'heading 'acceptable 'text: "Acceptable")
(statement-table 'heading 'pro 'text: "Pro") ; number of pro arguments
(statement-table 'heading 'con 'text: "Con") ; number of con arguments
(statement-table 'heading 'standard 'text: "Standard")
(statement-table 'heading 'content 'text: "Content")
(statement-table 'column 'id 'width: 50 'minwidth: 50 'stretch: 0)
(statement-table 'column 'status 'width: 80 'minwidth: 80 'stretch: 0)
(statement-table 'column 'acceptable 'width: 80 'minwidth: 80 'stretch: 0)
(statement-table 'column 'pro 'width: 25 'minwidth: 20 'stretch: 0)
(statement-table 'column 'con 'width: 25 'minwidth: 20 'stretch: 0)
(statement-table 'column 'standard 'width: 60 'minwidth: 60 'stretch: 0)
(statement-table 'column 'content 'width: 480 'minwidth: 480 'stretch: 1)

(tk/bind statement-table "<<TreeviewSelect>>" 
         (lambda ()
           (let* ((s1 (statement-table 'selection))
                  (p  (hashtable-ref *statements-table* (statement-table 'set s1 'content) #f)))
             (if p (load-arguments! p) ))))


; load-arguments!: statement -> void
(define (load-arguments! p)
  ; (printf "debug: p=~w~%" p)
  (if *current-argument-graph* 
      (let ((ag *current-argument-graph*)
            (load-arg! (lambda (arg) 
                         (let ((id (argument-id arg))
                               (title "") ; to do: add title to arguments
                               (direction (argument-direction arg))
                               (applicable (if (applicable? *current-argument-graph* arg)
                                               "yes"
                                               "no"))
                               (weight "") ; to do: replace context-compare with weights
                               (scheme (argument-scheme arg)))
                           (argument-tree 'insert ""
                                          'end
                                          'values: (list id applicable title direction weight scheme))))))
        
        ; clear the argument tree
        (argument-tree 'delete (argument-tree 'children ""))
        (for-each load-arg! (append (pro-arguments ag p) 
                                    (con-arguments ag p))))
      (printf "debug: no *current-argument-graph*~%")))


(define argument-tree-frame
  (argument-graph-panel 'create-widget 'ttk::labelframe
                        'text: "Arguments"
                        'padding: 2))
(define argument-tree
  (argument-tree-frame 'create-widget 'ttk::treeview
                        'columns: '(id applicable title direction weight scheme)
                        'height: 5
                        'show: '(headings)))

(argument-tree 'heading 'id 'text: "ID")
(argument-tree 'heading 'title 'text: "Title")
(argument-tree 'heading 'direction 'text: "Pro/Con")
(argument-tree 'heading 'applicable 'text: "Applicable")
(argument-tree 'heading 'weight 'text: "Weight")
(argument-tree 'heading 'scheme 'text: "Scheme")


(argument-tree 'column 'id 'width: 50 'minwidth: 50 'stretch: 0)
(argument-tree 'column 'title 'width: 250 'minwidth: 150 'stretch: 0)
(argument-tree 'column 'direction 'width: 50 'minwidth: 50 'stretch: 0)
(argument-tree 'column 'applicable 'width: 100 'minwidth: 100 'stretch: 0)
(argument-tree 'column 'weight 'width: 100 'minwidth: 100 'stretch: 0)
(argument-tree 'column 'scheme 'width: 250 'minwidth: 100 'stretch: 1)

(tk/bind argument-tree "<<TreeviewSelect>>" 
         (lambda ()
           (if *current-document*
               (let* ((s1 (argument-tree 'selection))
                      (id (argument-tree 'set s1 'id)))
                (load-premises! (string->symbol id))))))

(define (load-premises! arg-id)
  (if *current-argument-graph* 
      (let* ((ag *current-argument-graph*)
             (arg (get-argument ag arg-id))
             
             (load-premise! (lambda (p) 
                              (let ((statement (statement-formatted (premise-atom p)))
                                    (polarity (if (premise-polarity p) "+" "-"))
                                    (holds (if (holds? ag p) "yes" "no"))
                                    (role (premise-role p))
                                    (type (cond ((ordinary-premise? p) "ordinary")
                                                ((exception? p) "exception")
                                                ((assumption? p) "assumption"))))
                                (premise-table 'insert ""
                                               'end
                                               'values: (list holds type role polarity statement))))))
        
        ; first clear the premise-table
        ; (printf "arg-id=~w; arg=~w~%" arg-id arg)
        (premise-table 'delete (premise-table 'children ""))
        (if arg (for-each load-premise! (argument-premises arg))))))

(define premise-table-frame
  (argument-graph-panel 'create-widget 'ttk::labelframe
                        'text: "Premises"
                        'padding: 2))

(define premise-table
  (premise-table-frame 'create-widget 'ttk::treeview
                       'columns: '(holds type role polarity statement)
                       'height: 5
                       'show: '(headings)))

(premise-table 'heading 'polarity 'text: "+/-")
(premise-table 'heading 'holds 'text: "Holds")
(premise-table 'heading 'type 'text: "Type")
(premise-table 'heading 'role 'text: "Role")
(premise-table 'heading 'statement 'text: "Statement")
(premise-table 'column 'polarity 'width: 30 'minwidth: 20 'stretch: 0)
(premise-table 'column 'holds 'width: 50 'minwidth: 50 'stretch: 0)
(premise-table 'column 'type 'width: 100 'minwidth: 100 'stretch: 0)
(premise-table 'column 'role 'width: 150 'minwidth: 150 'stretch: 0)
(premise-table 'column 'statement 'width: 470 'minwidth: 250 'stretch: 1)


(tk/grid argument-graph-table-frame 'row: 0 'column: 0 'rowspan: 4 'padx: "2m" 'pady: "2m")
(tk/grid argument-graph-table 'row: 0 'column: 0  'padx: "2m" 'pady: "2m")
(tk/grid search-panel 'row: 0 'column: 1 'sticky: 'we 'padx: "2m" 'pady: "0m")
(tk/grid statement-table-frame 'row: 1 'column: 1 'padx: "2m" 'pady: "0m")
(tk/grid statement-table 'row: 0 'column: 0  'padx: "2m" 'pady: "2m")
(tk/grid argument-tree-frame 'row: 2 'column: 1 'padx: "2m" 'pady: "0m")
(tk/grid argument-tree 'row: 0 'column: 0 'padx: "2m" 'pady: "2m")
(tk/grid premise-table-frame 'row: 3 'column: 1 'padx: "2m" 'pady: "0m")
(tk/grid premise-table 'row: 0 'column: 0  'padx: "2m" 'pady: "2m")

; Layout the panels of the notebook

(nb1 'add argument-graph-panel 
     'text: "Arguments" 
     'underline: 0)


(nb1 'add rule-panel 
     'text: "Rules" 
     'underline: 0)

                            
(tk/wm 'deiconify tk)
(tk/wm 'resizable tk #f #f)
(tk-event-loop tk)