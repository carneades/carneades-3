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

(module persuasion-dialog mzscheme
  (require "atom.scm")
  (require "argument.ss")
  (require "commitment-store.ss")
  (require "proof-standards.scm")
  (require (lib "datatype.ss" "eopl"))
  
  (provide (all-defined)) ; change to hide internals
  
  ; type role = proponent | opponent
  
  ; make-party : string role -> party
  (define-struct party (name role))
  
  ; messages defined using an algebraic datatype
  (define-datatype message message?
      (claim (atom atom?))     ; assert
      (withdraw (atom atom?))
      (question (atom atom?))
      (concede (atom atom?))
      (accept (atom atom?))    ; decision
      (reject (atom atom?))    ; decision
      (support (conclusion atom?) (argument argument?)) ; pro
      (rebut (conclusion atom?) (argument argument?)) ; con
      (reveal (arg-id symbol?) (premise atom?)) ; e.g. applicabity assumptions
      (define (symbol symbol?) (value any?)))   ; for reifying values in atoms
   
  ; moves/speech acts/performatives
  ; make-move : party message -> move
  (define-struct move (party message))
 
  ; type code = success | failure
  ; make-move-result:  code string dialog-state -> move-result
  (define-struct move-result (code description dialog-state))
  
  (define-struct dialog-state
    (parties           ; party-list
     claim             ; proposition or void
     history           ; move list, in reverse chronological order
     commitments       ; commitment-store
     environment       ; namespace mapping symbols in atoms to values, for reification.
     ))
  
  ; Select the preponderance proof standard. 
  ; Ask Doug if another proof standard would be more appropriate for 
  ; presuasion dialogs.
  (define acceptable? (make-acceptable-predicate preponderance?))
  (define valid? (make-valid-predicate preponderance?))
  
  ; goal-state? : dialog-state -> boolean
  ; A goal state has been reached if the claim is not void and is acceptable.
  (define (goal-state? ds) 
      (and (not (void? (dialog-state-claim ds)))
           (acceptable? (dialog-state-claim ds))))
  
  ; perform : dialog-state move -> move-result
  ; The perform function is an interpreter for the messages of this dialog type.
  ; It checks whether the preconditions of the move, depending on the kind of
  ; message, are met. If the preconditions are met, the move is executed and a successful move-result
  ; containing a dialog state representing the effects of the move is returned. 
  ; If the preconditions are not met, a failed move-result is returned, together
  ; with an unchanged dialog state, i.e. a dialog state equal to the one passed
  ; to the perform function.

  (define (perform ds m) 
    (cases message (move-message m)
      (claim (atom) (do-claim ds m atom))    
      (withdraw (atom) (do-withdraw ds m atom))
      (question (atom) (do-question ds m atom))
      (concede (atom) (do-concede ds m atom))
      (accept (atom) (do-accept ds m atom))
      (reject (atom) (do-reject ds m atom))
      (support (conclusion argument) (do-support ds m conclusion argument))
      (rebut (conclusion argument) (do-rebut ds m conclusion argument))
      (reveal (arg-id premise) (do-reveal ds m arg-id premise))
      (define (symbol value) (do-define ds m symbol value))))

  ;; Move Definitions
  
  ; do-claim : dialog-state move atom -> move-result
  ; Precondition - The claim (i.e. thesis) of the dialog has not been asserted.
  ; Effects - Asserts the claim and adds it to the speaker's commitment store.  
  (define (do-claim ds m a) 
    (if (void? (dialog-state-claim ds)) ; claim has not been asserted yet
        (make-move-result 'success 
                          (format "Claim asserted: ~a" (atom->list a))
                          (make-dialog-state (dialog-state-parties ds)
                                             (make-proposition 'issue a null null)
                                             (cons m (dialog-state-history ds))
                                             (add-commitment (dialog-state-commitments ds) 
                                                             (move-party m)
                                                             a)
                                             (dialog-state-environment ds)))
        (make-move-result 'failure "Claim previously asserted." ds)))
  
  ; do-withdraw : dialog-state move atom -> move-result
  ; Precondition - The atom is a member of the speaker's commitment store. 
  ; Effects - Removes the atom from the speaker's commitment store.
  (define (do-withdraw ds m a) 
    (if (committed? (dialog-state-commitments ds) (move-party m) a)
        (make-move-result 'success
                          (format "Commitment withdrawn: ~a is no longer commited to ~a."
                                  (party-name (move-party m))
                                  (atom->list a))
                          (make-dialog-state (dialog-state-parties ds)
                                             (dialog-state-claim ds)
                                             (cons m (dialog-state-history ds))
                                             (remove-commitment (dialog-state-commitments ds) 
                                                                (move-party m)
                                                                a)
                                             (dialog-state-environment ds)))
        (make-move-result 'failure 
                          (format "~a was not committed to ~a."
                                  (party-name (move-party m))
                                  (atom->list a))
                          ds)))
  
  ; do-question: dialog-state move atom -> move-result
  ; Preconditions: The atom is a presumption of some argument and the speaker
  ; is not commited to the atom.
  ; Effect: An issue is made out of the presumption.
  (define (do-question ds m a)
    (cond ((committed? (dialog-state-commitments ds) (move-party m) a)
           (make-move-result 'failure 
                          (format "~a is committed to ~a."
                                  (party-name (move-party m))
                                  (atom->list a))
                          ds))
          (...)))  ; START HERE
             
  
  (define (do-concede ds m a) 'tbd)
  (define (do-accept ds m a) 'tbd)
  (define (do-reject ds m a) 'tbd)
  
  ; do-support : dialog-state party atom argument -> move-result
  (define (do-support ds m c a) 'tbd)
  
  (define (do-rebut ds m c a) 'tbd)
  (define (do-reveal ds id p) 'tbd)
  (define (do-define ds s v) 'tbd)
  
  ; to do: check for cycles when adding arguments; 
  
) ; dialog