
(module PiersonPost mzscheme
  
  (require "../src/statement.scm")
  (require "../src/argument.scm")
  (require "../src/argument-diagram.scm")
  (require (prefix table: (planet "table.ss" ("soegaard" "galore.plt" 3 4))))

  ; The Pierson vs. Post case.  Used to illustrate the use of
  ;   a scheme for "practical reasoning" in legal argument.
  ; See Atkinson, Bench-Capon, McBurney, "Arguing About Cases
  ;  as Practical Reasoning",  ICAIL05.  The following is a recontruction
  ; of the arguments in the published opinion, not a reconstruction
  ; of the reconstruction in Katie's, Trevor's and Peter's paper. 
  
  ; The full text of the decision can be found at:
  ; http://www.saucyintruder.org/pages/pierson.html 
  
  ;  Judge Tompkins Opinion, for the majority 

  (define not-property 
    "Post, by pursuing the fox, 
did not acquire property 
in the fox.") 
	
  (define possession-required 
    "Property rights in wild 
animals may be acquired 
only by possession.")
  
  (define foxes-are-wild 
    "Foxes are wild animals.")
  
  (define no-possession
    "Post did not have 
possession of the fox.")
 
  (define pursuit-not-sufficient 
    "Pursuit is not sufficient 
to acquire possession.")
  
  (define justinian 
    "Justinian's 
Institutes")
  
  (define fleta "Fleta")
  
  (define bracton "Bracton")
  
  (define actual-possession-required 
    "Actual corporal 
possession is required.")
  
  (define puffendorf "Puffendorf")
  
  (define bynkershoek "Bynkershoek")
     
  (define mortally-wounded-deemed-possessed  
    "Pursuit is sufficient to obtain 
possession when the animal
is mortally wounded.")
  
  (define barbeyrac "Barbeyrac")
  
  (define grotius "Grotius")
  
  (define mortally-wounded 
    "The fox was 
mortally wounded.")
  
  (define land-owner-has-possession ; warrant
    "The owner of land pursuing a 
livelihood with animals on his land is
deemed to have possession of the animals.")
  
  (define livelihood-on-own-land 
    "Post was pursing his livelihood 
on his own land")
  
  (define keeble "Keeble") ; backing

  (define certainty     ; action
    "A bright-line rule creates
legal certainty, preserving 
peace and order.")
  
  (define order         ; value
    "Peace and order is
an important social value.")
 
  (define c0 
    (make-context (table:make-ordered statement-compare) ; status table
                  (lambda (statement) 'se) ; default standard: scintilla
                  (lambda (arg1 arg2) 0))) ; no priorities
  
  (define-argument a1 (pro not-property 
                           (pr possession-required)
                           (pr no-possession)
                           (pr foxes-are-wild)))

  (define-argument a2 (pro no-possession (pr pursuit-not-sufficient)))
  (define-argument a3 (pro pursuit-not-sufficient (am justinian)))
  (define-argument a4 (pro pursuit-not-sufficient (am fleta)))
  (define-argument a5 (pro pursuit-not-sufficient (am bracton)))
  (define-argument a6 (pro no-possession (pr actual-possession-required)))
  
  (define-argument a7 (pro actual-possession-required (am puffendorf)))
  (define-argument a8 (pro puffendorf (am bynkershoek)))
  
  (define-argument a9
    (con actual-possession-required 
        (pr mortally-wounded-deemed-possessed)
        (pr mortally-wounded)))
  
  (define-argument a10 (pro mortally-wounded-deemed-possessed (am grotius)))
  
  (define-argument a11 (pro mortally-wounded-deemed-possessed (am barbeyrac)))
  
  (define-argument a12 
    (con actual-possession-required
         (pr land-owner-has-possession)
         (pr livelihood-on-own-land)))

  (define-argument a13 (pro land-owner-has-possession (am keeble)))
  
  ; teleological argument 
  (define-argument a14  
    (pro actual-possession-required 
         (pr certainty)   ; policy/action
         (pr order)))     ; value promoted
 
  (define tompkins 
    (assert empty-argument-graph
            (list a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14)))
 
  (define c1 (accept c0 (list foxes-are-wild possession-required certainty order)))
  ; (view tompkins c1)

  ; Judge Livingston's dissent.
  
  (define chased-by-big-dogs 
    "The fox was being 
chased by large hounds.")
 
  (define deemed-mortally-wounded 
    "A noxious animal being chased 
by large hounds shall be 
deemed mortally wounded.")
  
  (define protecting-farmers   ; value
    "Protecting farmers is an 
important social value.")
  
  (define encourage-hunting    ; policy/action
    "Encouraging hunting 
helps protect farmers 
from noxious animals.")
  
  (define foxes-are-noxious
    "Foxes are noxious animals.")
  
  (define admitted-in-the-pleadings
    "It is admitted in 
the pleadings that
a fox is a wild
and noxious beast.")
  
  (define-argument a15
    (pro mortally-wounded
         (pr deemed-mortally-wounded)
         (pr chased-by-big-dogs)
         (pr foxes-are-noxious)))
  
  (define-argument a16
    (pro deemed-mortally-wounded 
         (pr protecting-farmers)
         (pr encourage-hunting)))
  
  (define-argument a17
    (pro foxes-are-noxious (am admitted-in-the-pleadings)))
 
  (define livingston (assert empty-argument-graph (list a15 a16 a17)))
  (define c2 (accept c1 (list chased-by-big-dogs)))
  
  (define both (assert tompkins (list a15 a16 a17)))
 ; (view both c2)
 
 ; (diagram livingston c2)
 ; (view livingston c2)
  
  (define fig4-args (assert empty-argument-graph (list a9 a10 a11)))
  (define fig5-args (assert empty-argument-graph (list a12 a13)))

)