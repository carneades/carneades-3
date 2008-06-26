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
 (carneades evidence)
 
 ; Implementation of an argumentation scheme for arguments from evidence, in particular testimonial evidence.
 ; Questions and answers represent statements of the form (<predicate> <subject> <object>), i.e. triples as in RDF.
 ; The <predicate> and <subject> must be instantiated when asking the question.  The only information 
 ; requested are the <object> values.  An <object> may be any Scheme datum.  Propositional logic is simulated
 ; with triples of the form (<proposition> #t <boolean>).   For example, a propositional version of "It is not the 
 ; case that Tweety flies." can be represented (tweety-flies #t #f).
 
 
 ; To do:
 ;
 ; - critical questions for testimonial evidence
 ; - default values for questions, possibly
 ; - addition data types for questions, in particular dates.
 ; - extend LKIF.scm to support the importing and exporting of forms, questions and answers.
 
 (export make-witness witness-name make-question question-predicate question-type
         question-cardinality question-text make-form form-questions form-help
         make-testimony testimony-witness testimony-forms testimony-answers 
         testimony-statements get-form get-question get-answer already-asked? 
         generate-arguments-from-testimony)
 
 (import (rnrs)
         (carneades stream)
         (carneades unify)
         (carneades statement)
         (carneades argument-search)
         (prefix (carneades argument) argument:)
         (carneades lib srfi format)
         (prefix (carneades lib srfi lists) list:)
         (carneades lib match)
         (carneades gensym))
 
 (define (printf format-string . args)
   (apply format `(,(current-output-port)  ,format-string ,@args)))
 
 (define null '())
 
 (define-record-type witness 
   (fields name ; string
           ; other attributes can be defined in record types
           ; which inherit from this one
           ))      
 
 (define-record-type question
   (fields predicate   ; symbol, e.g. 'income, 'age, 'gender
           type        ; 'symbol | 'number | 'boolean | 'string
           cardinality ; 'one | 'many 
           text        ; A natural language version of the question, with an optional "format" slot for 
           ; subject.  e.g. "What is ~A's age?"
           )
   (sealed #t))
 
 ; valid-input?: question (list-of datum) -> boolean
 ; Checks that a datum is of the type expected by the question
 (define (valid-input? question values)
   (define (check-type v)
     (case (question-type question)
       ((symbol) (symbol? v))
       ((number) (number? v))
       ((boolean) (boolean? v))
       ((string) (string? v))
       (else #f)))
   (case (question-cardinality question)
     ((one) (and (list? values) (= 1 (length values)) (check-type (car values))))
     ((many) (and (list? values) (list:every check-type values)))
     (else #f)))
 
 ; If the answer is closed, the absence of a value in the list of values can be used to 
 ; generate a con argument, using the "closed-world assumption".  A better term for this here
 ; might be the "closed-world assertion". Notice that this assertion is not global, but made
 ; for each property of each object, separately.  For example, if the 
 ; issue is (p s o), p is closed for s and o is not in the list of values for p of s, then
 ; an the testimony can be used to make an argument con (p s o).
 ; If the user does not know the answer to a question, and thus cannot provide some (or any) values,
 ; an answer record for the question will be nonetheless associated with the question in 
 ; the answers table of the user's testimony.  This answer will be open (not closed) and the 
 ; list of values provided may be empty. The presence of the answer in the answer table records
 ; that the question has been asked, however many values were supplied.
 (define-record-type answer
   (fields subject     ; symbol
           question    ; question
           values      ; (list-of datum)
           closed      ; boolean, true if all values have been provided
           )
   (sealed #t))
 
 ; form: if any question in the form is asked, they all are asked, in the given order.
 ; Used to produce a more structured dialogue.
 (define-record-type form
   (fields questions  ; question list, at most one question per predicate
           help       ; HTML, in SXML format.
           )
   (sealed #t))
 
 (define-record-type testimony%
   (fields witness    ; witness
           forms      ; form hashtable, mapping predicates to forms
           answers    ; mutable answer hashtable of hashtables, 
           ; mapping predicates to subjects to answers
           )
   (sealed #t))
 
 (define testimony? testimony%?)
 (define testimony-witness testimony%-witness)
 (define testimony-forms testimony%-forms)
 (define testimony-answers testimony%-answers)
 
 ; make-testimony: witness (list-of form) -> testimony
 ; assumption: there is at most one question in the form for each predicate
 (define (make-testimony witness forms)
   (let ((forms-tbl (make-eq-hashtable)))
     (for-each (lambda (form) 
                 (for-each (lambda (question)
                             (hashtable-set! forms-tbl 
                                              (question-predicate question)
                                              form))
                           (form-questions form)))
               forms)
     (make-testimony% witness forms-tbl (make-eq-hashtable))))
 
 ; record-answer!: testimony question symbol (list-of datum) boolean -> void
 ; Record the answer(s) to the question. 
 (define (record-answer! testimony question subject values closed)
   (let* ((subj-tbl (hashtable-ref (testimony-answers testimony)
                                   (question-predicate question) 
                                   #f)))
     (if (not subj-tbl)
         (begin (set! subj-tbl (make-eq-hashtable))
                (hashtable-set! (testimony-answers testimony)
                                (question-predicate question)
                                subj-tbl)))
     (hashtable-set! subj-tbl subject (make-answer subject 
                                                   question
                                                   values
                                                   closed))))
 
 
 ; get-form: testimony symbol -> form | #f
 (define (get-form testimony predicate)
   (hashtable-ref (testimony-forms testimony) predicate #f))
 
 ; get-question: testimony symbol -> question | #f
 (define (get-question testimony predicate)
   (let ((form (get-form testimony predicate)))
     (and form
          (list:find (lambda (question) 
                       (eq? (question-predicate question)
                            predicate))
                     (form-questions form)))))
 
 ; get-answer: testimony question symbol -> answer
 (define (get-answer testimony question subject)
   (let* ((subj-tbl (if (hashtable-contains? (testimony-answers testimony)
                                             (question-predicate question))
                        (hashtable-ref (testimony-answers testimony)
                                       (question-predicate question) 
                                       #f)
                        (let ((v (make-eq-hashtable)))
                          (hashtable-set! (testimony-answers testimony)
                                          (question-predicate question)
                                          v)
                          v)))
          (answer (hashtable-ref subj-tbl subject #f)))
     (or answer 
         (begin (ask! testimony question subject)
                (get-answer testimony question subject)))))
 
 (define (already-asked? testimony question subject)
   (let ((subj-tbl (hashtable-ref (testimony-answers testimony)
                                   (question-predicate question) 
                                   (make-eq-hashtable))))
     (answer? (hashtable-ref subj-tbl subject #f))))
 
 ; answer-value->statement: predicate subject value -> statement
 (define (answer-value->statement predicate subject value)
   (case value
     ((#t)
      (if (eq? subject #t)
          predicate
          `(,predicate ,subject)))
     ((#f)
      (if (eq? subject #t)
          `(not ,predicate)
          `(not (,predicate ,subject))))
     (else `(,predicate ,subject ,value))))
 
 ; testimony-statements: testimony -> (list-of statement)
 (define (testimony-statements testimony)
   (define (hashtable-values ht) ; -> (list-of values)
     (call-with-values (lambda () (hashtable-entries ht))
                       (lambda (ks vs) (vector->list vs))))
   (apply append  
          (map (lambda (subject)
                 (apply append 
                        (map (lambda (answer)
                               (map (lambda (value)
                                      (answer-value->statement 
                                       (question-predicate (answer-question answer))
                                       (answer-subject answer)
                                       value))
                                    (answer-values answer)))
                             (hashtable-values subject))))
               (hashtable-values (testimony-answers testimony)))))
   
 
 ; ask-one-question!: testimony question symbol -> void
 ; side effect: updates the testimony to record the answer
 (define (ask-one-question! testimony question subject)
   (let loop ()
     (if (eq? subject #t)
         (printf (question-text question))
         (printf (question-text question) subject))
     (printf " ")
     (let* ((v (read)))
       (match v
         ('none (record-answer! testimony question subject null #f))
         (('some . values) 
          (if (valid-input? question values)
              (record-answer! testimony question subject values #f)
              (begin (printf "Invalid input. Please try again.~%")
                     (loop))))
         (('all . values) 
          (if (valid-input? question values)
              (record-answer! testimony question subject values #t)
              (begin (printf "Invalid input. Please try again.~%")
                     (loop))))
         (_ (begin (printf "Invalid input. Please try again.~%")
                   (loop)))))))
 
 ; ask!: testimony question symbol -> void
 ; If the question has not already been asked for this subject, 
 ; asks all open and unasked questions in the form, in order.
 ; The testimony is updated as a side effect.
 (define (ask! testimony question subject)
   (if (not (already-asked? testimony question subject))
       (for-each 
        (lambda (question) 
          (if (not (already-asked? testimony question subject))
              (ask-one-question! testimony question subject)))
        (form-questions (get-form testimony (question-predicate question))))))
 
 
 ; type generator : statement argument-graph substitutions -> 
 ;                  (stream-of (pair-of argument substitutions ))
 
 ; dispatch: statement testimony subs -> (stream-of response)
 (define (dispatch goal testimony subs)
   (define (f direction predicate subject object)
     (if (not (ground? (list predicate subject))) ; check that predicate and subject are bound
         (stream) ; fail
         (let ((question (get-question testimony predicate)))
           (if (not question)
               (stream) ; fail
               (let ((answer (get-answer testimony question subject)))
                 (case direction
                   ((con) 
                    (if (eq? subject #t) 
                        ; negative propositional query
                        (if (and (eq? (question-type question) 'boolean)
                                 (eq? (question-cardinality question) 'one)
                                 (member #f (answer-values answer)))
                            (let ((arg (argument:make-argument 
                                        (gensym 'a) ; id
                                        'con
                                        predicate        ; conclusion
                                        null             ; premises
                                        (string-append   ; scheme
                                         "testimony: " 
                                         (witness-name (testimony-witness testimony))))))
                              (stream (make-response subs arg)))
                            (stream)) ; else fail
                        ; else negative first-order query
                        (if (and (answer-closed answer)
                                 (not (member (subs object)
                                              (answer-values answer))))
                            (let ((arg (argument:make-argument 
                                        (gensym 'a)      ; id
                                        'con
                                        (statement-atom goal) ; conclusion
                                        null             ; premises
                                        (string-append   ; scheme
                                         "testimony: " 
                                         (witness-name (testimony-witness testimony))))))
                              (stream (make-response subs arg)))
                            (stream)))) ; else fail
                   
                   ((pro)
                    (if (eq? subject #t) 
                        ; positive propositional query
                        (if (and (eq? (question-type question) 'boolean)
                                 (eq? (question-cardinality question) 'one)
                                 (member #t (answer-values answer)))
                            (let ((arg (argument:make-argument
                                        (gensym 'a)      ; id
                                        'pro
                                        predicate        ; conclusion
                                        null             ; premises
                                        (string-append   ; scheme
                                         "testimony: " 
                                         (witness-name (testimony-witness testimony))))))
                              (stream (make-response subs arg)))
                            (stream)) ; else fail
                        ; else positive first-order query
                        (stream-flatmap 
                         
                         (lambda (value) 
                           (let ((subs2 (unify* goal (answer-value->statement predicate subject value) 
                                                subs 
                                                (lambda (t) t) 
                                                (lambda (msg) #f)
                                                #f)))
                             (if subs2 
                                 (let ((arg (argument:make-argument 
                                             (gensym 'a)      ; id
                                             'pro
                                             ; conclusion: 
                                             goal 
                                             ; premises:
                                             null
                                             ; scheme: 
                                             (string-append
                                              "testimony: " 
                                              (witness-name (testimony-witness testimony))))))
                                   (stream (make-response subs2 arg)))
                                 (stream)))) ; else fail, as unification failed
                         (list->stream (answer-values answer)))))
                   (else (stream)))))))) ; fail
   
   (let ((p (subs goal)))
     (match p 
       (('not (predicate subject))
        (f 'con predicate subject #t))
       (('not (predicate subject object))
        (f 'con predicate subject object))
       (('not proposition)
        (f 'con proposition #t #t))
       ((predicate subject)
        (f 'pro predicate subject #t))
       ((predicate subject object)
        (f 'pro predicate subject object))
       (proposition            
        (f 'pro proposition #t #t)))))
 
 ; type generator: statement state -> (stream-of response)
 
 ; generate-arguments-from-testimony: testimony -> generator
 (define (generate-arguments-from-testimony testimony)
   (lambda (goal state) (dispatch goal testimony (state-substitutions state))))
 
 
 ) ;end of module
