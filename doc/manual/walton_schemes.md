
# Builtin Argumentation Schemes

This chapter presents the argumentation schemes included with the
distribution of the Carneades system. The system is pre-configured to
use these schemes, but you can configure the system to use other
schemes, or modify these schemes to meet your requirements.

The argumentation schemes are shown here in pseudocode for
readabililty. See the [Modeling Argumentation
Schemes](#modeling-argumentation-schemes) chapter for a
description of the syntax used to formally define the schemes.

The schemes can be viewed online, using Carneades, by clicking on the
"Theory" link near the top of an argument outline page, if the project
has not been configured to use some other schemes.

Most of the schemes here are derived from the book "Argumentation Schemes"
[@Walton:2008]. The schemes for arguments from credible source and
practical reasoning are based on [@Wyner:2012] and [@Atkinson:2007],
respectively.

The schemes from these sources been modified to fit the Carneades
computational model of argument. For example, generic critical
questions which undermine premises, undercut the argument or rebut its
conclusion, have been omitted, since these critical questions apply to
all defeasible arguments in Carneades.

All the argumentation schemes presented here are defeasible *unless*
they have been explicitly declared to be strict.

## Argument from Position to Know

~~~
id: position-to-know  

conclusion: S

premises:
	major: W is in a position to know about things in a certain
		subject domain D.
	minor: W asserts that S is true.
	domain: S is in domain D.
	
exceptions:
	CQ1: W is dishonest.
~~~

## Argument from Credible Source

~~~
id:  credible-source

conclusion: S

premises:
	source: W is a credible source about domain D.
	assertion: W asserts S.
	domain: S is in domain D.

exceptions:
	CQ1: W is biased.
	CQ2: W is dishonest.
	CQ3: Other credible sources disagree with S.
~~~

## Argument from Witness Testimony

~~~
id: witness-testimony

conclusion: S

premises: 
	position to know: W is in a position to know about things in a 
		certain subject domain D.
  in-domain: Domain D contains the statement S.
	truth-telling: Witness W believes S to be true.
	minor: W asserts that S is true.
   
assumptions:
	CQ1: S is internally consistent.
	
exceptions:
	CQ2: S is inconsistent with the facts.
	CQ3: S is inconsistent with the testimony of other witnesses.
	CQ4: W is biased.
	CQ5: S is implausible.
~~~

## Argument from Expert Opinion

~~~
id: expert-opinion

conclusion: S

premises: 
   major: Source E is an expert in subject domain D.
   domain: Domain D contains the statement S..
   minor: E asserts that S is true.
   
exceptions:
	CQ1: E is untrustworthy.
	CQ2: S is inconsistent with the testimony of other witnesses.

assumptions:
	CQ3: S is based on evidence.
~~~

## Argument from Analogy

~~~
id: analogy

conclusion: S

premises:
	major: Case C1 is similar to the current case.
   case: S is true in case C1.
   minor: E asserts that A is true.
   
exceptions:
	CQ1: There are relevant differences between case C1 and the 
		current case.
	CQ2: S is false in case C1, which is more on point 
		than case	C2.
~~~
    
## Argument from Precedent

~~~
id: precedent

conclusion: S

premises:
	major: Case C1 is similar to the current case.
	ratio: Rule R is the ratio decidendi of case C1.
	conclusion: Rule R has conclusion S.
   
exceptions:
	CQ1: There are relevant differences between case C1 and the 
		current case.
	CQ2: Rule R is inapplicable in this case.
~~~

## Argument from Verbal Classification

~~~
id: definition-to-verbal-classification

strict: true

conclusion: O is an instance of class G.

premises:
	individual: O satisfies definition D.
	classification: Objects which satisfy definition D are
		classified as instances of class G.
~~~
    
## Argument from Definition to Verbal Classification

~~~
id: definition-to-verbal-classification

strict: true

conclusion: O is an instance of class G.

premises:
   individual: O satisfies definition D.
   classification: Objects which satisfy definition D are
		classified as instances of class G.
~~~
    
## Defeasible Modus Ponens

~~~
id: defeasible-modus-ponens

conclusion: B

premises:
	major: If A is true then presumably B is also true.
	minor: A
~~~
    
## Argument from an Established Rule

~~~
id: established-rule

conclusion: C

premises:
	major: Rule R has conclusion C.
	minor: Rule R is applicable.

assumptions:
	CQ1: Rule R is valid.
~~~

## Argument from Positive Consequences

~~~
id: positive-consequences

conclusion: Action A should be performed.

premises:
	major: Performing action A would have positive consequences.
~~~
    
## Argument from Negative Consequences

~~~
id: negative-consequences

conclusion: Action A should not be performed.

premises:
	major: Performing action A would have negative consequences.
~~~

## Argument from Practical Reasoning

~~~
id: practical-reasoning

conclusion: A1 should be performed.

premises:
	circumstances: S1 is currently the case.
	action: Performing A1 in S1 would bring about S2.
	goal: G would be realized in S2.
	value: Achieving G would promote V.

assumptions:
	CQ1: V is indeed a legitimate value.
	CQ2: G is a worthy goal.
	CQ3: Action A1 is possible.

exceptions:
	CQ4: There exists an action that, when performed in S1, would 
		bring about S2 more effectively than A1.
	CQ5: There exists an action that, when performed in S1,  would 
		realize G more effectively than A1.
	CQ6: There exists an action that, when performed in S1, would 
		promote V more effectively than A1.
	CQ7: Performing A1 in S1 would have side-effects 
		which demote V or some other value.
~~~	

## Argument from Cause to Effect.

~~~
id: cause-to-effect

conclusion: Event E2 will occur.

premises:
	minor: An event E1 has occurred.
	major: Event E1 causes event E2.

exceptions:
	CQ1: An event E3 interferred with E1.
~~~


## Argument from Correlation to Cause

~~~
id: correlation-to-cause

conclusion: Event E1 causes event E2.

premises:
	major: Events E1 and E2 are correlated.

assumptions:
	CQ1: There exists a theory explaining the correlation 
		between E1 and E2.

exceptions:
	CQ2: E3 causes E1 and E2.
~~~

## Argument from Sunk Costs

~~~
id: sunk-costs

conclusion: Action A should be performed.

premises:
	costs: The costs incurred performing A thus far are C.
	waste: The sunk costs of C are too high to waste.

assumptions:
	CQ1: Action A is feasible.
~~~
    
## Argument from Appearance


~~~
id: appearance

conclusion: O is an instance of class C.

premises:
	minor: O looks like a C.
~~~

## Argument from Ignorance

~~~
id: ignorance

conclusion: S

premises:
	major: S would be known if it were true.
	minor: S is known to be true.

exceptions:
	CQ1: The truth of S has not been investigated.
~~~


## Argument from Abduction

~~~
id: abduction

conclusion: H

premises:
	observation: observed ?S
	explanation: Theory T1 explains S.
	hypothesis: T1 contains H as a member.

exceptions:
	CQ1: T2 is a more coherent explanation than T1 of S.
~~~
    
## Ethotic Argument

~~~
id: ethotic

conclusion: S

premises:
	assertion: P asserts that S is true.
	trustworthiness: P is trustworthy.
~~~

## Slippery Slope Argument

This version of the slippery slope scheme is intended to be used
together with the argument from negative consequences schema, to
derive the conclusion that the action should not be performed.

Notice that the scheme is represented by *two* Carneades schemes, one
for the base case and one for the inductive step.

### Base Case
~~~
id: slippery-slope-base-case

conclusion: Performing action A would have negative consequences

premises:
	realization: Performing A would realize event E.
	horrible costs: Event E would have horrible costs.
~~~

### Inductive Step

~~~
id: slippery-slope-inductive-step

conclusion: Event E1 would have horrible consequences

premises:
	causation: Event E1 causes E2.
	consequences: Event E2 would haves horrible consequences.
~~~
