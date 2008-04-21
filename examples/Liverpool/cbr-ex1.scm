(module cbr-ex1 mzscheme
  
  (require (lib "argument.scm" "carneades"))
  (require (lib "argument-builtins.scm" "carneades"))
  (require (lib "rule.scm" "carneades"))
  (require (lib "case.scm" "carneades"))
  (require (lib "shell.scm" "carneades"))
  (require (planet "test.ss" ("schematics" "schemeunit.plt")))
  (require (planet "text-ui.ss" ("schematics" "schemeunit.plt")))
  
  (define f101 (make-factor 'info-trade-secret 'plaintiff #f))
  (define f102 (make-factor 'efforts-to-maintain-secrecy 'plaintiff f101))
  (define f120 (make-factor 'info-legitimately-obtained-or-obtainable 'defendant #f))
  (define f104 (make-factor 'info-valuable 'plaintiff f101))
  (define f105 (make-factor 'info-known-or-available 'defendant f101))
  (define f108 (make-factor 'info-available-elsewhere 'defendant f105))
  (define f111 (make-factor 'questionable-means 'plaintiff f120))
  (define f114 (make-factor 'confidential-relationship 'plaintiff #f))
  (define f115 (make-factor 'notice-of-confidentiality 'plaintiff f114))
  (define f122 (make-factor 'efforts-to-maintain-secrecy-vis-a-vis-defendant 'plaintiff f102))
  (define f1 (make-factor 'disclosure-in-negotiations 'defendant f122))
  (define f2 (make-factor 'bribed-employee 'plaintiff f111))
  (define f4 (make-factor 'agreed-not-to-disclose 'plaintiff f122))
  (define f6 (make-factor 'security-measures 'plaintiff f102))
  (define f7 (make-factor 'brought-tools 'plaintiff #f))
  (define f10 (make-factor 'secrets-disclosed-to-outsiders 'defendant f105))
  (define f12 (make-factor 'outsider-disclosures-restricted 'plaintiff f105))
  (define f15 (make-factor 'unique-product 'plaintiff f104))
  (define f16 (make-factor 'info-reverse-engineerable 'defendant f108))
  (define f21 (make-factor 'knew-info-confidential 'plaintiff f115))
  (define f25 (make-factor 'information-reverse-engineered 'defendant f111))
  (define f26 (make-factor 'used-deception 'plaintiff f111))
  (define f27 (make-factor 'disclosure-in-public-forum 'defendant f105))
  
  (define factors (list f1 f2 f4 f6 f7 f10 f12 f15 f16 f21 f25 f26 f27  f101 f102 f104 f105 
                        f108 f111 f114 f115 f120 f122))
  
  ; cases from [Wyner & Bench-Capon, 2007]
  (define vanilla (make-case "Vanilla" 'plaintiff (list f1 f15)))
  (define bribe (make-case "Bribe" 'plaintiff (list f1 f2 f15)))
  (define deceit (make-case "Deceit" 'plaintiff (list f1 f15 f26)))
  (define disclose (make-case "Disclose" 'plaintiff (list f1 f10 f15)))
  (define restrict (make-case "Restrict" 'plaintiff (list f1 f10 f12 f15)))
  (define bribe2 (make-case "Bribe2" 'plaintiff (list f1 f2 f15 f25)))
  (define rev (make-case "Reverse" 'plaintiff (list f1 f15 f25)))
  (define announce (make-case "Announce" 'defendant (list f1 f15 f27)))
  
  ; HYPO cases
  (define kfc (make-case "KFC" 'plaintiff (list f6 f16)))
  (define american-precision (make-case "American Precision" 'plaintiff (list f7 f16 f21)))
  (define digital-development (make-case "Digital Development" 'plaintiff (list f1 f6 f15 f21)))
  (define speciner (make-case "Speciner" 'defendant (list f1 f16)))
  (define yokana (make-case "Yokana" 'defendant (list f7 f16)))
  (define bryce (make-case "Bryce" 'plaintiff (list f1 f6 f21)))
  (define space-aero (make-case "Space Aero" 'plaintiff (list f1 f15)))
  (define smith (make-case "Smith" 'defendant (list f1)))
  (define mason (make-case "Mason" 'undecided (list f1 f6 f15 f16 f21)))
  
  (define wyner-cases (list vanilla bribe deceit disclose restrict bribe2 rev announce))
  ; hypo cases: from less to more on point
  (define hypo-cases (list yokana bryce space-aero smith 
                           american-precision kfc digital-development speciner))
  
  ; (define hypo-cases (list american-precision yokana))
  
  (define wyner-cb (make-casebase 'trade-secret-violation factors wyner-cases))
  (define hypo-cb (make-casebase 'trade-secret-violation factors hypo-cases))
  
  ; Initial contexts for some test "current" cases, with their
  ; accepted statements copied from factors of cases
  
  (define (make-context-from-case case)
    (accept default-context (case-statements case)))
  
  (define c-vanilla (make-context-from-case vanilla))
  (define c-mason (make-context-from-case mason)) 
  (define c-announce (make-context-from-case announce))
  
  ; A simple rulebase, to test whether rules and cases can be used together.
  (define rb1 (rulebase  
               (rule r1 (if has-patent unique-product))
               (rule r2 (if (not used-deception) honest))))
  
  ; cbr-engine: integer integer case-base context -> statement -> (stream-of argument-state)
  (define (cbr-engine max-nodes max-turns case-base context)
    (make-engine* max-nodes max-turns context
                 (list builtins 
                       ; (generate-arguments-from-rules rb1 critical-questions)
                       (generate-arguments-from-cases case-base))))
  
  (define wyner-engine (cbr-engine 50 4 wyner-cb null))
  (define hypo-engine (cbr-engine 50 4 hypo-cb null))
 
 
  ; to do: further and more systematic tests
  
  (define tests
    (test-suite
     "CBR tests"
     (test-true "q1" (some-acceptable? 'trade-secret-violation (cbr-engine 50 4 wyner-cb c-vanilla)))
     (test-true "q2" (some-acceptable? 'trade-secret-violation (cbr-engine 50 4 wyner-cb c-mason)))
     ; re q3 and q4 below: neither claim is defensible since the burden of proof is on the proponent,
     ; and the opponent can distinguish every precedent for either claim.
     (test-true "q3" (not (some-acceptable? 'trade-secret-violation (cbr-engine 50 4 hypo-cb c-mason))))
     (test-true "q4" (not (some-acceptable? '(not trade-secret-violation) (cbr-engine 50 4 hypo-cb c-mason))))
     (test-true "q5" (all-acceptable? '(not trade-secret-violation) (cbr-engine 50 4 wyner-cb c-announce)))
     ))
  
  (test/text-ui tests)
  
  ; (ask 'trade-secret-violation (cbr-engine 50 4 wyner-cb c-vanilla))  
  ; (show1 '(not trade-secret-violation) (cbr-engine 50 4 hypo-cb c-mason))
    
  ) ; module end 