#!r6rs

(import (rnrs base)
        (carneades shell)
        (carneades argument-builtins)
        (carneades rule)
        (carneades lib srfi lightweight-testing))

(define null '())

(define ita_savings
  
  (rulebase
   
   
   (rule §1-1a-ita
         (if (and (subject-listed-in-1-1 ?x)
                  (resident-in-italy ?x)   
                  (pays-or-secures-interest ?x ?y)
                  (individual ?y)
                  (beneficial-owner ?y)
                  (resident-in-another-member-state ?y)
                  (operates-as-debtor-or-charged-to-pay ?x))
             (shall-communicate-the-information-concerning-the-payment-to-Revenue-Agency ?x)))
   
   (rule §1-1a_01-ita
         (if (or (bank ?x)
                 (investment-firm ?x)
                 (poste-italiane-spa ?x)
                 (security-investment-fund-management-company ?x)
                 (financial-company ?x)
                 (trust-company ?x))
             (subject-listed-in-1-1 ?x)))
   
   (rule §1-1a_02-ita
         (if (or (pays-interest-to ?x ?y)
                 (secures-interest-payment-for-immediate-benefit-of ?x ?y))
             (pays-or-secures-interest ?x ?y)))
   
   (rule §1-1a_03-ita
         (if (or (operates-as-debtor-of-the-debt-claim-producing-the-interest ?x)
                 (operates-as-charged-by-debtor-to-pay-interest-or-secure-the-payment-of-interest ?x)
                 (operates-as-charged-by-beneficial-owner-to-pay-interest-or-secure-the-payment-of-interest ?x))
             (operates-as-debtor-or-charged-to-pay ?x)))
   
   
   (rule §1-1b1-ita
         (if (and (individual ?x)
                  (resident-in-Italy ?x)
                  (pays-or-secures-interest ?x ?y)
                  (pays-or-secures-payment-of-interests-for-professional-or-commercial-purposes ?x)
                  (individual ?y)
                  (beneficial-owner ?y)
                  (resident-in-another-member-state ?y)
                  (operates-as-debtor-or-charged-to-pay ?x))
             (shall-communicate-the-information-concerning-the-payment-to-Revenue-Agency ?x)))
   
   
   (rule §1-1b2-ita
         (if (and (subject ?x)
                  (resident-in-Italy ?x)
                  (pays-or-secures-interest ?x ?y)
                  (pays-or-secures-payment-of-interests-for-professional-or-commercial-purposes ?x)
                  (individual ?y)
                  (beneficial-owner ?y)
                  (resident-in-another-member-state ?y)
                  (operates-as-debtor-or-charged-to-pay ?x))
             (shall-communicate-the-information-concerning-the-payment-to-Revenue-Agency ?x)))
   
   
   (rule §1-1c-ita
         (if (and (permanent-organization-in-Italy-of-non-residents ?x)
                  (pays-or-secures-interest ?x ?y)
                  (pays-or-secures-payment-of-interests-for-professional-or-commercial-purposes ?x)
                  (individual ?y)
                  (beneficial-owner ?y)
                  (resident-in-another-member-state ?y)
                  (operates-as-debtor-or-charged-to-pay ?x))
             (shall-communicate-the-information-concerning-the-payment-to-Revenue-Agency ?x)))
   
   
   (rule §1-1d-ita
         (if (and (individual ?x)
                  (interest-payment ?i)
                  (receives-as-final-recipient ?x ?i))
             (beneficial-owner ?x)))
   
   
   (rule §1-3-01-ita
         (if (and (entity ?x)
                  (beneficial owner ?y)
                  (receives-interest-payment-for-the-benefit-of ?x ?y)
                  (resident-in-Italy ?x)
                  (not (legal person ?x))
                  (not (subject-whose-profits-are-taxed-under-determination-arrangements-of-business-taxation ?x))
                  (not (authorized-UCITS ?x))
                  (not (and (choose-to-be-treated-as-UCITS ?x)
                            (has-certificate-issued-by-Revenue-Agency ?x))))
             (shall-communicate-the-information-concerning-the-payment-to-Revenue-Agency ?x)))
   
   (rule §1-3-02-ita
         (if (and (entity ?x)
                  (beneficial owner ?y)
                  (receives-interest-secured-for-the-benefit-of ?x ?y)
                  (resident-in-Italy ?x)
                  (not (legal person ?x))
                  (not (subject-whose-profits-are-taxed-under-determination-arrangements-of-business-taxation ?x))
                  (not (authorized-UCITS ?x))
                  (not (and (choose-to-be-treated-as-UCITS ?x)
                            (has-certificate-issued-by-Revenue-Agency ?x))))
             (shall-communicate-the-information-concerning-the-payment-to-Revenue-Agency ?x)))
   
   
   
   (rule §2_1_a_ita
         (if (and (interest-paid-or-credited-to-an-account ?x)
                  (interest-relating-to-debt-claims-of-every-kind ?x)
                  (not (penalty-charge-for-late-payment ?x)))
             (interest-payment-following2_1_a ?x)))  
   
   (rule §2_1_b_ita
         (if (or (interest-accrued-at-the-sale-of-debts-claims-of-every-kind ?x)
                 (interest-accrued-at-the-refund-of-debts-claims-of-every-kind ?x)
                 (interest-accrued-at-the-redemption-of-debts-claims-of-every-kind ?x))
             (interest-payment-following2_1_b ?x)))    
   
   
   (rule §2_1_c_ita
         (if (or (income-deriving-directly-from-interest-payments ?x)
                 (and (income-deriving-from-interest-payments-through ?x ?z)
                      (and (entity ?z)
                           (beneficial owner ?y)
                           (or (receives-interest-payment-for-the-benefit-of ?z ?y)
                               (receives-interest-secured-for-the-benefit-of ?z ?y))
                           (or (resident-in-Italy ?z)
                               (resident-in-another-member-state ?z))
                           (not (legal person ?z))
                           (not (subject-whose-profits-are-taxed-under-determination-arrangements-of-business-taxation ?z))
                           (not (authorized-UCITS ?z))
                           (not (and (choose-to-be-treated-as-UCITS ?z)
                                     (has-certificate-issued-by-Revenue-Agency ?z))))
                      (or (distributed-by-authorized-UCITS ?x)
                          (distributed-by-entity-which-qualifies-for-the-option-to-be-treated-as-UCITS ?x)
                          (distributed-by-undertaking-for-collective-investment-established-outside-the-territory-of-Treaty ?x))))
             (interest-payment-following2_1_c ?x)))
   
   (rule §2_1_d_ita
         (if (and (or (income-realised-upon-sale-or-refund-or-redemption-of-shares-in ?x ?e)
                      (income-realised-upon-sale-or-refund-or-redemption-of-units-in ?x ?e))
                  (or (authorized-UCITS ?e)
                      (entity-which-qualifies-for-the-option-to-be-treated-as-UCITS ?e)
                      (undertaking-for-collective-investment-established-outside-the-territory-of-Treaty ?e))
                  (and (invests-directly-or-indirectly-more-than-40percent-assets-in ?e ?c)
                       (interest-paid-or-credited-to-an-account ?c)
                       (interest-relating-to-debt-claims-of-every-kind ?c)
                       (not (penalty-charge-for-late-payment ?c))))
             (interest-payment-following2_1_d ?x)))
   
   (rule §2_ita
         (if (or (interest-payment-following2_1_a ?x)
                 (interest-payment-following2_1_b ?x)
                 (interest-payment-following2_1_c ?x)
                 (interest-payment-following2_1_d ?x))
             (interest-payment ?x)))       
   
   ;--------------------------------------------   
   ; rule facts
   ;--------------------------------------------
   
   (rule* facts
          (interest-accrued-at-the-sale-of-debts-claims-of-every-kind money_1)
          (individual Brown)
          (receives-as-final-recipient Brown money_1)
          (financial-company Acme)
          (resident-in-italy Acme)   
          (pays-or-secures-interest Acme Brown)
          (resident-in-another-member-state Brown)
          (operates-as-debtor-or-charged-to-pay Acme)
          )
   
   
   ))


; type question = excluded | priority | valid

(define (engine max-nodes max-turns critical-questions)
  (make-engine max-nodes max-turns 
               (list (generate-arguments-from-rules ita_savings critical-questions)
                     builtins)))


(check (all-acceptable? '(beneficial-owner ?x) (engine 50 3 null)) => #t)
(check-report)

; (ask '(beneficial-owner ?x) (engine 50 3 null))
; (show1 '(beneficial-owner ?x) (engine 50 3 null))

