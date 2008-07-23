#!r6rs

(import (rnrs)
        (carneades dlp)
        (carneades system))

(ontology expression
          
          (define-concept expression:Communicated_Attitude (some expression:states expression:Expression))
          
          (define-concept expression:Expression (some expression:medium expression:Medium))
          
          (define-concept expression:Intention (some expression:intended_by action:Agent))
          
          (define-concept expression:Belief (some expression:believed_by action:Agent))
          
          (define-primitive-concept action:Agent  (all expression:holds lkif-top:Mental_Entity))
          
          (define-primitive-concept expression:Evidence (and role:Epistemic_Role (some role:played_by (and expression:Proposition (some expression:attitude expression:Belief) (some role:plays expression:Observation)))))
          
          (define-concept expression:Qualification (some expression:qualifies expression:Qualified))
          
          (define-primitive-concept action:Action (and (all action:actor (some expression:believes (and expression:Belief (some expression:towards (some role:plays expression:Expectation))))) (all action:actor (some expression:intends expression:Intention))))
          
          (define-primitive-concept expression:Surprise  expression:Observation)
          
          (define-concept expression:Evaluative_Attitude (some expression:evaluates expression:Evaluative_Proposition))
          
          (define-concept expression:Declaration (some expression:declares expression:Expression))
          
          (define-concept expression:Assertion (some expression:asserts expression:Expression))
          
          (define-primitive-concept expression:Expectation (and role:Epistemic_Role(and (some role:played_by (and expression:Proposition (some expression:attitude expression:Belief))) (all role:played_by (all role:plays (not expression:Observation))))))
          
          (define-concept expression:Medium (some expression:bears expression:Expression))
          
          (define-concept expression:Statement_In_Writing (some expression:states (and expression:Expression (some expression:medium expression:Document))))
          
          (define-primitive-concept role:Epistemic_Role )
          
          (define-primitive-concept expression:Argument (and expression:Reason (some role:played_by (and expression:Expression (some expression:attitude expression:Belief)))))
          
          (define-primitive-concept expression:Cause  role:Epistemic_Role)
          
          (define-primitive-concept action:Reaction  (all action:actor (and action:Agent (some expression:observes (and expression:Belief (some expression:qualifies (some role:plays (and expression:Observation (some role:played_by action:Action)))))))))
          
          (define-concept expression:Promise (some expression:promises expression:Expression))
          
          (define-concept expression:Qualified (some expression:qualified_by expression:Qualification))
          
          (define-concept expression:Evaluative_Proposition (some expression:evaluated_by expression:Evaluative_Attitude))
          
          (define-primitive-concept expression:Exception (and role:Epistemic_Role (some role:played_by expression:Proposition)))
          
          (define-primitive-concept expression:Assumption (and role:Epistemic_Role (some role:played_by (and expression:Proposition (some expression:attitude expression:Belief)))))
          
          (define-primitive-concept expression:Fact (and role:Epistemic_Role (some role:played_by (and expression:Proposition (some expression:attitude expression:Belief) (some role:plays expression:Observation)))))
          
          (define-concept expression:Observation (some role:played_by (some expression:attitude (some expression:observer action:Agent))))
          
          (define-primitive-concept expression:Document (and expression:Medium (all expression:bears (and expression:Expression (some expression:stated_by expression:Statement_In_Writing)))))
          
          (define-primitive-concept expression:Proposition (and lkif-top:Mental_Object (all expression:attitude expression:Propositional_Attitude)))
          
          (define-primitive-concept owl:Thing )
          
          (define-primitive-concept expression:Desire  expression:Propositional_Attitude)
          
          (define-concept expression:Propositional_Attitude (some expression:towards expression:Proposition))
          
          (define-primitive-concept expression:Lie  expression:Assertion)
          
          (define-primitive-concept expression:Problem  expression:Observation)
          
          (define-primitive-concept action:Creation )
          
          (define-primitive-concept lkif-top:Mental_Entity )
          
          (define-concept expression:Speech_Act (some process:creation expression:Communicated_Attitude))
          
          (define-concept lkif-top:Mental_Object (some expression:held_by action:Agent))
          
          (define-primitive-concept expression:Reason (and role:Epistemic_Role (some role:played_by (and expression:Proposition (some expression:attitude expression:Belief)))))
          
          (define-primitive-role expression:observer :domain expression:Belief :range expression:Belief :parents( expression:believed_by))
          
          (define-primitive-role expression:author :parents( expression:utterer))
          
          (define-primitive-role expression:intended_by :domain expression:Intention :range expression:Intention :parents( expression:held_by))
          
          (define-primitive-role role:played_by)
          
          (define-primitive-role expression:held_by :domain lkif-top:Mental_Object :range lkif-top:Mental_Object)
          
          (define-primitive-role process:creation)
          
          (define-primitive-role expression:promises :domain expression:Promise :range expression:Promise :parents( expression:states))
          
          (define-primitive-role expression:observes :domain action:Agent :range action:Agent :parents( expression:believes))
          
          (define-primitive-role expression:qualitatively_comparable :symmetric t :domain expression:Qualified :range expression:Qualified)
          
          (define-primitive-role expression:qualified_by :domain expression:Qualified :range expression:Qualified)
          
          (define-primitive-role expression:evaluates :domain expression:Evaluative_Attitude :range expression:Evaluative_Attitude :parents( expression:qualifies expression:towards))
          
          (define-primitive-role expression:asserted_by :domain expression:Expression :range expression:Expression :parents( expression:stated_by))
          
          (define-primitive-role expression:evaluated_by :domain expression:Evaluative_Proposition :range expression:Evaluative_Proposition :parents( expression:attitude expression:qualified_by))
          
          (define-primitive-role expression:believes :domain action:Agent :range action:Agent :parents( expression:holds))
          
          (define-primitive-role expression:states :domain expression:Communicated_Attitude :range expression:Communicated_Attitude :parents( expression:towards))
          
          (define-primitive-role expression:asserts :domain expression:Assertion :range expression:Assertion :parents( expression:states))
          
          (define-primitive-role expression:believed_by :domain expression:Belief :range expression:Belief :parents( expression:held_by))
          
          (define-primitive-role expression:addressee :domain expression:Communicated_Attitude :range expression:Communicated_Attitude)
          
          (define-primitive-role expression:utterer :domain expression:Communicated_Attitude :range expression:Communicated_Attitude :parents( expression:held_by))
          
          (define-primitive-role expression:holds :domain action:Agent :range action:Agent)
          
          (define-primitive-role expression:declared_by :domain expression:Expression :range expression:Expression :parents( expression:stated_by))
          
          (define-primitive-role expression:evaluatively_comparable :symmetric t :domain expression:Evaluative_Proposition :range expression:Evaluative_Proposition :parents( expression:qualitatively_comparable))
          
          (define-primitive-role expression:towards :domain expression:Propositional_Attitude :range expression:Propositional_Attitude)
          
          (define-primitive-role expression:intends :domain action:Agent :range action:Agent :parents( expression:holds))
          
          (define-primitive-role expression:utters :domain action:Agent :range action:Agent :parents( expression:holds))
          
          (define-primitive-role expression:attitude :domain expression:Proposition :range expression:Proposition)
          
          (define-primitive-role expression:bears :domain expression:Medium :range expression:Medium)
          
          (define-primitive-role expression:medium :domain expression:Expression :range expression:Expression)
          
          (define-primitive-role expression:declares :domain expression:Declaration :range expression:Declaration :parents( expression:states))
          
          (define-primitive-role expression:stated_by :parents( expression:attitude))
          
          (define-primitive-role expression:qualifies :domain expression:Qualification :range expression:Qualification)
          
          (define-primitive-role action:actor)
          
          (define-primitive-role role:plays)
          
          (define-primitive-role expression:promised_by :domain expression:Expression :range expression:Expression :parents( expression:stated_by))
          
          )