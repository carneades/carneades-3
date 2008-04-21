(module EU_savings mzscheme
  
  (require (lib "shell.scm" "carneades"))
  (require (lib "argument-builtins.scm" "carneades"))
  (require (lib "rule.scm" "carneades"))
  (require (planet "test.ss" ("schematics" "schemeunit.plt" 2 4)))
  (require (planet "text-ui.ss" ("schematics" "schemeunit.plt" 2)))
  
  (define savings
    
    (rulebase
     
     ;--------------------------------------------   
     ; article 2 : beneficial owner
     ;--------------------------------------------
     
     (rule §-2-1-pa1-EUSD
           (if (and (individual ?x)
                    (interest-payment ?c)
                    (receives ?x ?c))
               (beneficial-owner ?x)))
     
     
     (rule §-2-1-pa2-EUSD
           (if (and (individual ?x)
                    (interest-payment ?c)
                    (secured-for ?c ?x))
               (beneficial-owner ?x)))
     
     
     (rule §-2-1-exception-a-EUSD
           (if (and (individual ?x)
                    (provides-evidence ?x (paying-agent-following-4-1 ?x)))
               (and (excluded §-2-pa1-EUSD (beneficial-owner ?x))
                    (excluded §-2-pa2-EUSD (beneficial-owner ?x)))))
     
     
     (rule §-2-1-exception-b1-EUSD
           (if (and (individual ?x)
                    (provides-evidence ?x (legal-person ?y))
                    (provides-evidence ?x (acts-on-behalf-of ?x ?y)))
               (and (excluded §-2-pa1-EUSD (beneficial-owner ?x))
                    (excluded §-2-pa2-EUSD (beneficial-owner ?x)))))
     
     
     (rule §-2-1-exception-b2-EUSD
           (if (and (individual ?x)
                    (provides-evidence ?x (entity-taxed-on-its-profits ?y))
                    (provides-evidence ?x (acts-on-behalf-of ?x ?y)))
               (and (excluded §-2-pa1-EUSD (beneficial-owner ?x))
                    (excluded §-2-pa2-EUSD (beneficial-owner ?x)))))
     
     
     (rule §-2-1-exception-b3-EUSD
           (if (and (individual ?x)
                    (provides-evidence ?x (UCITS-authorised ?y))
                    (provides-evidence ?x (acts-on-behalf-of ?x ?y)))
               (and (excluded §-2-pa1-EUSD (beneficial-owner ?x))
                    (excluded §-2-pa2-EUSD (beneficial-owner ?x)))))
     
     
     (rule §-2-1-exception-b4-EUSD
           (if (and (individual ?x)
                    (provides-evidence ?x (entity-referred-to-in-4-2 ?y))
                    (provides-evidence ?x (acts-on-behalf-of ?x ?y))
                    (economic-operator ?z)
                    (discloses-name-and-address-of-entity ?x ?z)
                    (competent-authority ?m)
                    (transmits-info ?z ?m))
               (and (excluded §-2-pa1-EUSD (beneficial-owner ?x))
                    (excluded §-2-pa2-EUSD (beneficial-owner ?x)))))
     
     
     (rule §-2-1-exception-c-EUSD
           (if (and (individual ?x)
                    (provides-evidence ?x (beneficial-owner ?y))
                    (provides-evidence ?x (acts-on-behalf-of ?x ?y))
                    (competent-authority ?m)
                    (discloses-identity-of-beneficiary ?x ?m))
               (and (excluded §-2-pa1-EUSD (beneficial-owner ?x))
                    (excluded §-2-pa2-EUSD (beneficial-owner ?x)))))
     
     
     (rule §-2-2-001-EUSD
           (if (and (paying-agent ?x)
                    (individual y)
                    (interest-payment ?c)
                    (receives ?y ?c)
                    (has information ?x (not (beneficial-owner ?y)))
                    (not (applies (rule §-2-1-exception-a-EUSD)(beneficial-owner ?y)))
                    (not (applies (rule §-2-1-exception-b1-EUSD)(beneficial-owner ?y)))
                    (not (applies (rule §-2-1-exception-b2-EUSD)(beneficial-owner ?y)))
                    (not (applies (rule §-2-1-exception-b3-EUSD)(beneficial-owner ?y)))
                    (not (applies (rule §-2-1-exception-b4-EUSD)(beneficial-owner ?y)))
                    (not (can-identify-another-beneficiary ?x)))
               (beneficial-owner ?y)))
     
     
     
     (rule §-2-2-002-EUSD
           (if (and (paying-agent ?x)
                    (individual y)
                    (interest-payment ?c)
                    (secured-for ?y ?c)
                    (has-information ?x (not (beneficial-owner ?y)))
                    (not (applies (rule §-2-1-exception-a-EUSD)(beneficial-owner ?y)))
                    (not (applies (rule §-2-1-exception-b1-EUSD)(beneficial-owner ?y)))
                    (not (applies (rule §-2-1-exception-b2-EUSD)(beneficial-owner ?y)))
                    (not (applies (rule §-2-1-exception-b3-EUSD)(beneficial-owner ?y)))
                    (not (applies (rule §-2-1-exception-b4-EUSD)(beneficial-owner ?y)))
                    (not (can-identify-another-beneficiary ?x)))
               (beneficial-owner ?y)))
     
     ;--------------------------------------------   
     ; article 4 : paying agent
     ;--------------------------------------------
     
     (rule §-4-1-001-EUSD
           (if (and (economic-operator ?x)
                    (beneficial-owner ?y)
                    (pays-interest-for-the-benefit-of ?x ?y)
                    (debt-claim-that-produces-interest ?c)
                    (debtor-of ?x ?c)
                    (has-debt-with ?x ?y))
               (paying-agent ?x)))
     
     
     (rule §-4-1-002-EUSD
           (if (and (economic-operator ?x)
                    (beneficial-owner ?y)
                    (pays-interest-for-the-benefit-of ?x ?y)
                    (entity ?z)
                    (has-debt-with ?z ?y)
                    (charged-by ?x ?z))
               (paying-agent ?x)))
     
     
     (rule §-4-1-003-EUSD
           (if (and (economic-operator ?x)
                    (beneficial-owner ?y)
                    (pays-interest-for-the-benefit-of ?x ?y)
                    (charged-by ?x ?y))
               (paying-agent ?x)))
     
     
     (rule §-4-1-004-EUSD
           (if (and (economic-operator ?x)
                    (beneficial-owner ?y)
                    (secures-payment-of-interest-for-the-benefit-of ?x ?y)
                    (debt-claim-that-produces-interest ?c)
                    (debtor-of ?x ?c)
                    (has-debt-with ?x ?y))
               (paying-agent ?x)))
     
     
     (rule §-4-1-005-EUSD
           (if (and (economic-operator ?x)
                    (beneficial-owner ?y)
                    (secures-payment-of-interest-for-the-benefit-of ?x ?y)
                    (entity ?z)
                    (has-debt-with ?z ?y)
                    (charged-by ?x ?z))
               (paying-agent ?x)))
     
     
     (rule §-4-1-006-EUSD
           (if (and (economic-operator ?x)
                    (beneficial-owner ?y)
                    (secures-payment-of-interest-for-the-benefit-of ?x ?y)
                    (charged-by ?x ?y))
               (paying-agent ?x)))
     
     
     (rule §-4-2-001-EUSD
           (if (and (entity ?x)
                    (beneficial-owner ?y)
                    (member-state ?c)
                    (established-in ?x ?c)
                    (interest ?i)
                    (is-paid-to ?i ?x)
                    (is-paid-for-the-benefit-of ?i ?y))
               (and (paying-agent ?x)
                    (entity-following4-2 ?x))))
     
     
     (rule §-4-2-002-EUSD
           (if (and (entity ?x)
                    (beneficial-owner ?y)
                    (member-state ?c)
                    (established-in ?x ?c)
                    (interest ?i)
                    (is-secured-for- ?i ?x)
                    (is-paid-for-the-benefit-of ?i ?y))
               (and (paying-agent ?x)
                    (entity-following4-2 ?x))))
     
     
     (rule §-4-2-a-EUSD
           (if (and (entity ?x)
                    (beneficial-owner ?y)
                    (has-reason-to-believe ?y (legal-person ?x))
                    (has-reason-to-believe ?y (not (avoin-yhtiöa ?x)))
                    (has-reason-to-believe ?y (not (kommandiittiyhtiö ?x)))
                    (has-reason-to-believe ?y (not (handelsbolag ?x)))
                    (has-reason-to-believe ?y (not (kommanditbolag ?x))))
               (and (excluded §-4-2-001-EUSD (paying-agent ?x))
                    (excluded §-4-2-002-EUSD (paying-agent ?x)))))
     
     
     (rule §-4-2-b-EUSD
           (if (and (entity ?x)
                    (beneficial-owner ?y)
                    (has-reason-to-believe ?y (has-profits ?x ?p))
                    (has-reason-to-believe ?y (are-taxed-under-general-arrangements-for-business-taxation ?p)))
               (and (excluded §-4-2-001-EUSD (paying-agent ?x))
                    (excluded §-4-2-002-EUSD (paying-agent ?x)))))
     
     
     (rule §-4-2-c-EUSD
           (if (and (entity ?x)
                    (beneficial-owner ?y)
                    (has-reason-to-believe ?y (UCITS-recognised ?x)))
               (and (excluded §-4-2-001-EUSD (paying-agent ?x))
                    (excluded §-4-2-002-EUSD (paying-agent ?x)))))
     
     
     (rule §-4-2-003-EUSD
           (if (and (economic-operator ?x)
                    (paying-agent ?y)
                    (member-state ?a)
                    (member-state ?b)
                    (established-in ?x ?a)
                    (established-in ?y ?b)
                    (different ?a ?b)
                    (pays-interest-to ?x ?y)
                    (competent-autorithy ?c)
                    (competent-authority ?d)
                    (established-in ?c ?a)
                    (established-in ?d ?b))
               (and (shall-communicate-name ?x ?y)
                    (shall-communicate-address ?x ?y)
                    (shall-communicate-interest ?x ?y)
                    (shall-give-communication-to ?y ?c)
                    (shall-give-communication-to ?c ?d))))
     
     
     (rule §-4-2-004-EUSD
           (if (and (economic-operator ?x)
                    (paying-agent ?y)
                    (member-state ?a)
                    (member-state ?b)
                    (established-in ?x ?a)
                    (established-in ?y ?b)
                    (different ?a ?b)
                    (secures-interest-for ?x ?y)
                    (competent-autorithy ?c)
                    (competent-authority ?d)
                    (established-in ?c ?a)
                    (established-in ?d ?b))
               (and (shall-communicate-name ?x ?y)
                    (shall-communicate-address ?x ?y)
                    (shall-communicate-interest ?x ?y)
                    (shall-give-communication-to ?y ?c)
                    (shall-give-communication-to ?c ?d))))
     
     
     (rule §-4-3-001-EUSD
           (if (and (entity-following4-2 ?x)
                    (economic operator ?y)
                    (member-state ?s)
                    (competent-authority ?a)
                    (established-in ?x ?s)
                    (established-in ?a ?s)
                    (certificate ?c)
                    (issues ?a ?c)
                    (presents-to ?x ?y))
               (and (UCITS-recognised ?x)
                    (entity-which-qualify-for-the-option-under4-3 ?x))))
     
     
     (rule §-4-3-002-EUSD
           (if (and (entity ?x)
                    (member-state ?s)
                    (competent-authority ?a)
                    (established-in ?x ?s)
                    (established-in ?a ?s))
               (shall-lay-down-detailed-rules ?s)))
     
     
     (rule §-4-4-EUSD
           (if (and (entity-following4-2 ?x)
                    (economic-operator ?y)
                    (member-state ?s)
                    (established-in ?x ?s)
                    (established-in ?y ?s))
               (shall-take-measures-in-realtion-to ?s ?x)))   
     
     
     ;--------------------------------------------   
     ; article 5: competent authority
     ;--------------------------------------------
     
     (rule §-5-a-EUSD
           (if (and (authority ?x)
                    (member-state ?y)
                    (notifies-to-commission ?y ?x))
               (competent-authority ?x)))
     
     
     (rule §-5-b1-EUSD
           (if (and (authority ?x)
                    (not (member-state ?y))
                    (competent-authority-for-tax-conventions ?x))
               (competent-authority ?x)))
     
     
     (rule §-5-b2-EUSD
           (if (and (authority ?x)
                    (not (member-state ?y))
                    (not (competent-authority-for-tax-conventions ?x))
                    (competent-autority-for-certificates-of-residence-for-tax ?x))
               (competent-authority ?x)))
     
     
     ;--------------------------------------------   
     ; article 6: interest payment
     ;--------------------------------------------
     
     ;(rule §-6-1-a001-EUSD
     ;(if (and (interest-paid ?x)
     ;(debt-claim-of-every-kind ?y)
     ;(relating-to ?x ?y))
     ;(interest-payment-following6-1-a ?x)))
     
     
     ;(rule §-6-1-a002-EUSD
     ;(if (and (interest-credited-to-an-account ?x)
     ;(debt-claim-of-every-kind ?y)
     ;(relating-to ?x ?y))
     ;(interest-payment-following6-1-a ?x)))
     
     
     ;(rule §-6-1-a003-EUSD
     ;(if (income-from-government-securities ?x)
     ;(interest-payment-following6-1-a ?x)))
     
     
     (rule §-6-1-a004-EUSD
           (if (income-from-bonds ?x)
               (interest-payment-following6-1-a ?x)))
     
     
     (rule §-6-1-a005-EUSD
           (if (income-from-debentures ?x)
               (interest-payment-following6-1-a ?x)))
     
     
     (rule §-6-1-a006-EUSD
           (if (penalty-charges-for-late-payments ?x)
               (and (excluded §-6-1-001a-EUSD (interest-payment-following6-1-a ?x))
                    (excluded §-6-1-002a-EUSD (interest-payment-following6-1-a ?x))
                    (excluded §-6-1-003a-EUSD (interest-payment-following6-1-a ?x))
                    (excluded §-6-1-004a-EUSD (interest-payment-following6-1-a ?x))
                    (excluded §-6-1-005a-EUSD (interest-payment-following6-1-a ?x)))))
     
     (rule §-6-1-a007-EUSD
           (if (interest-payment-following6-1-a ?x)
               (interest-payment ?x)))
     
     (rule §-6-1-b001-EUSD
           (if (and (interest-accrued ?x)
                    (debt-claim-of-every-kind ?y)
                    (relating-to-the-sale-of ?x ?y))
               (interest-payment-following6-1-b ?x)))
     
     
     (rule §-6-1-b002-EUSD
           (if (and (interest-accrued ?x)
                    (debt-claim-of-every-kind ?y)
                    (relating-to-the-refund-of ?x ?y))
               (interest-payment-following6-1-b ?x)))
     
     
     (rule §-6-1-b003-EUSD
           (if (and (interest-accrued ?x)
                    (debt-claim-of-every-kind ?y)
                    (relating-to-the-redemption-of ?x ?y))
               (interest-payment-following6-1-b ?x)))
     
     
     (rule §-6-1-b004-EUSD
           (if (and (interest-capitalised ?x)
                    (debt-claim-of-every-kind ?y)
                    (relating-to-the-sale-of ?x ?y))
               (interest-payment-following6-1-b ?x)))
     
     
     (rule §-6-1-b005-EUSD
           (if (and (interest-capitalised ?x)
                    (debt-claim-of-every-kind ?y)
                    (relating-to-the-refund-of ?x ?y))
               (interest-payment-following6-1-b ?x)))
     
     
     (rule §-6-1-b006-EUSD
           (if (and (interest-capitalised ?x)
                    (debt-claim-of-every-kind ?y)
                    (relating-to-the-redemption-of ?x ?y))
               (interest-payment-following6-1-b ?x)))
     
     
     (rule §-6-1-b006-EUSD
           (if (interest-payment-following6-1-b ?x)
               (interest-payment ?x)))
     
     
     (rule §-6-1-c001-EUSD
           (if (and (income-deriving-directly-from-interest-payment ?x)
                    (UCITS-authorised ?y)
                    (distributes ?y ?x))
               (interest-payment-following6-1-c ?x)))
     
     
     (rule §-6-1-c002-EUSD
           (if (and (income-deriving-directly-from-interest-payment ?x)
                    (entity-which-qualify-for-the-option-under4-3 ?y)
                    (distributes ?y ?x))
               (interest-payment-following6-1-c ?x)))
     
     
     (rule §-6-1-c003-EUSD
           (if (and (income-deriving-directly-from-interest-payment ?x)
                    (undertaking-for-collective-investment ?y)
                    (territory-referred-to-in-7 ?t)
                    (established-outside ?y ?t)
                    (distributes ?y ?x))
               (interest-payment-following6-1-c ?x)))
     
     
     (rule §-6-1-c004-EUSD
           (if (and (income-from-interest-payment ?x)
                    (entity-following4-2 ?y)
                    (derives-through ?x ?y)
                    (UCITS-authorised ?z)
                    (distributes ?z ?x))
               (interest-payment-following6-1-c ?x)))
     
     
     (rule §-6-1-c005-EUSD
           (if (and (income-from-interest-payment ?x)
                    (entity-following4-2 ?y)
                    (derives-through ?x ?y)
                    (entity-which-qualify-for-the-option-under4-3 ?z)
                    (distributes ?z ?x))
               (interest-payment-following6-1-c ?x)))
     
     
     (rule §-6-1-c006-EUSD
           (if (and (income-from-interest-payment ?x)
                    (entity-following4-2 ?y)
                    (derives-through ?x ?y)
                    (undertaking-for-collective-investment ?z)
                    (territory-referred-to-in-7 ?t)
                    (established-outside ?z ?t)
                    (distributes ?z ?x))
               (interest-payment-following6-1-c ?x)))
     
     
     (rule §-6-1-c007-EUSD
           (if (interest-payment-following6-1-c ?x)
               (interest-payment ?x)))
     
     
     (rule §-6-1-d001-EUSD
           (if (UCITS-authorised ?x)
               (entity-referred-in-6-1-d ?x)))
     
     
     (rule §-6-1-d002-EUSD
           (if (entity-which-qualify-for-the-option-under4-3 ?x)
               (entity-referred-in-6-1-d ?x)))
     
     
     (rule §-6-1-d003-EUSD
           (if (and (undertaking-for-collective-investment ?x)
                    (territory-referred-to-in-7 ?t)
                    (established-outside ?x ?t))
               (entity-referred-in-6-1-d ?x)))
     
     
     (rule §-6-1-d004-EUSD
           (if (and (income ?x)
                    (entity-referred-in-6-1-d ?y)
                    (realised-upon-the-sale-of-share/units-of ?x ?y)
                    (debt-claim-of-every-kind ?d)
                    (invests-directly-more-than40%-of-its-assets-in ?y ?d))
               (interest-payment-following6-1-d ?x)))
     
     
     (rule §-6-1-d005-EUSD
           (if (and (income ?x)
                    (entity-referred-in-6-1-d ?y)
                    (realised-upon-the-refund-of-share/units-of ?x ?y)
                    (debt-claim-of-every-kind ?d)
                    (invests-directly-more-than40%-of-its-assets-in ?y ?d))
               (interest-payment-following6-1-d ?x)))
     
     
     (rule §-6-1-d006-EUSD
           (if (and (income ?x)
                    (entity-referred-in-6-1-d ?y)
                    (realised-upon-the-redemption-of-share/units-of ?x ?y)
                    (debt-claim-of-every-kind ?d)
                    (invests-directly-more-than40%-of-its-assets-in ?y ?d))
               (interest-payment-following6-1-d ?x)))
     
     
     (rule §-6-1-d007-EUSD
           (if (and (income ?x)
                    (entity-referred-in-6-1-d ?y)
                    (entity-referred-in-6-1-d ?z)
                    (realised-upon-the-sale-of-share/units-of ?x ?y)
                    (debt-claim-of-every-kind ?d)
                    (invests-more-than40%-of-its-assets-in ?y ?d)
                    (invests-through ?y ?z))
               (interest-payment-following6-1-d ?x)))
     
     
     (rule §-6-1-d008-EUSD
           (if (and (income ?x)
                    (entity-referred-in-6-1-d ?y)
                    (entity-referred-in-6-1-d ?z)
                    (realised-upon-the-refund-of-share/units-of ?x ?y)
                    (debt-claim-of-every-kind ?d)
                    (invests-more-than40%-of-its-assets-in ?y ?d)
                    (invests-through ?y ?z))
               (interest-payment-following6-1-d ?x)))
     
     
     (rule §-6-1-d009-EUSD
           (if (and (income ?x)
                    (entity-referred-in-6-1-d ?y)
                    (entity-referred-in-6-1-d ?z)
                    (realised-upon-the-redemption-of-share/units-of ?x ?y)
                    (debt-claim-of-every-kind ?d)
                    (invests-more-than40%-of-its-assets-in ?y ?d)
                    (invests-through ?y ?z))
               (interest-payment-following6-1-d ?x)))
     
     (rule §-6-1-d010-EUSD
           (if (and (interest-payment-following6-1-d ?x)
                    (interest-payment-following6-1-a ?y)
                    (derives-from ?x ?y))
               (interest-payment ?x)))
     
     ;; to be completed
     
     ;--------------------------------------------   
     ; article 7: covering by directive
     ;--------------------------------------------
     
     (rule §-7-EUSD
           (if (and (interest-payment ?x)
                    (paying-agent ?y)
                    (pays ?y ?x)
                    (territory-of-the-treaty ?t)
                    (established-in ?y ?t))
               (covered-by-directive ?x)))
     
     
     ;--------------------------------------------   
     ; article 8: information reporting by paying agent
     ;--------------------------------------------
     
     
     (rule §-8-EUSD
           (if (and (paying-agent ?y)
                    (beneficial-owner ?x)
                    (member-state ?m)
                    (member-state ?t)
                    (resides-in ?x ?m)
                    (resides-in ?y ?t)
                    (different ?m ?t)
                    (competent-authority ?a)
                    (necessary-infos ?i))
               (shall ?x (reported ?i ?a))))
     
     ;; to be completed
     
     ;--------------------------------------------   
     ;followings articles to be completed
     ;--------------------------------------------
     
     ;; to be completed
     
     ;--------------------------------------------   
     ; rule facts
     ;--------------------------------------------
     
     (rule* facts
            ; article 2 (beneficial owner)
            (individual Brown)
            ;(interest-payment money-item1)
            (receives Brown money-item1)
            ;
            ; article 4 (paying agent)
            (economic-operator Alfabank)
            (pays-interest-for-the-benefit-of Alfabank Brown)
            (debt-claim-that-produces-interest debt-item1)
            (debtor-of Alfabank debt-item1)
            (has-debt-with Alfabank Brown)
            ;
            ; article 5
            ;(authority Italian_Ministry_Finance)
            ;(member-state Italy)
            ;(notifies-to-commission Italy Italian_Ministry_Finance)
            ;(authority Algerian_Ministry_Finance)
            ;(not(member-state Algeria))
            ;(competent-authority-for-tax-conventions Algerian_Ministry_Finance)
            ;(authority Mali_Ministry_Finance)
            ;(not(member-state Mali))
            ;(not(competent-authority-for-tax-conventions Mali_Ministry_Finance))
            ;(competent-autority-for-certificates-of-residence-for-tax Mali_Ministry_Finance)
            ;
            ; article 6 (interest payment)
            (income-from-bonds money-item1)
            ;
            ; article 7 (covering by directive)
            ;(interest-payment money-item1)
            ;(paying-agent Smith)
            ;
            (pays Alfabank money-item1)
            (territory-of-the-treaty England)
            (established-in Alfabank England)
            )
     
     
     ))
  
  
  ; type question = excluded | priority | valid
  
  (define (engine max-nodes max-turns critical-questions)
    (make-engine max-nodes max-turns 
                 (list (generate-arguments-from-rules savings critical-questions)
                       builtins)))
   
  
  
  (define tests
    (test-suite
     "rules"
     (test-true "q1" (all-acceptable? '(covered-by-directive ?x) (engine 50 3 null)))
     
     ))
  
  (test/text-ui tests)
  
  ; (ask '(covered-by-directive ?x) (engine 50 3 null))
  ; (show1 '(covered-by-directive ?x) (engine 50 3 null))
  
  ) ; end of module

