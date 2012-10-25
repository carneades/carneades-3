
#  Modeling Policies and Argumentation Schemes

Policies and argumentation schemes are both represented using the same high-level scheme (rule) language in Carneades. We use the term "policy" here very broadly, to cover any kind of rule, norm, law, or regulation, whether or not the policy is currently valid and in force or proposed and under discussion.

Argumentation schemes generalize the notion of an inference rule to cover defeasible as well as strict reasoning patterns. We use the term "scheme" instead of "rule" in Carneades to emphasize that  the rules are usually defeasible. The Carneades scheme language is expressive enough to represent axiomatizations (i.e. axioms and inferences rules) of theories in many domains, including laws, regulations and policies, in addition to argumentation schemes per se. 

Computational models of theories have been called many things in computer science, including "knowledge bases" and "deep conceptual" or "semantic" models. We prefer the term "theory" to "knowledge-base" because the latter may suggest that theory is true, while the theory may be controversial or contested. 

The Carneades scheme language is similar to logic programming languages such as Prolog. Any Prolog rule ("clause") can be represented in Carneades in a straight-forward way. Carneades has some additional features for representing argumentation schemes, such as scheme variables ranging over atoms and a means to represent premise roles (e.g. "major", "minor"). Moreover, Carneades provides a way to annotate schemes along with meta-data and documentation, in multiple natural languages.  

The Carneades inference engine is able to automatically construct arguments from theories represented using the scheme language. Using a high-level declarative language for representing theories, and generating arguments from these theories, makes it easier for domain experts to represent and maintain domain theories.

Theories may organized in a hierarchical structure of sections and subsections, with schemes included at any level. Like schemes, the theory as a whole and each of its sections can be annotated with its own meta-data and natural language description. These features facilitate self-documenting and "isomorphic" modeling. The source text of the schemes, policies or legislation can be included within the model, in the same files, in such as way as to preserve the hierarchical structure of the sources. This makes it easier to maintain the model as the source documents are modified, since there can be a one-to-one correspondance between sections of the source text and sections of the model.

Theories are represented in text files, using the scheme language. The scheme language is an executable knowledge-representation language, with its own well-defined semantics and an inference engine implementing this semantics.  It is not intended to be used as an "interchange format" for exporting and importing theories among diverse formalisms with varied semantics.  

The semantics of argumentation schemes are formally defined by mapping instantiations of argumentation schemes to argument graphs and, in turn, mapping these graphs to Dung abstract argumentation frameworks [@Dung1995], using Prakken's ASPIC+ model of structured argument [@Prakken:2010a] in a way which preserves the Carneades model of proof standards [@GordonPrakkenWalton:2007a]. (*Note: Write and then cite here a technical report with the formal definitions.*)

## Practical Reasoning Argumentation Scheme Example

In this section we illustrate how to use the language to represent a version of the scheme for arguments from practical reasoning.

A domain theory is represented by first defining a *language* (dictionary) of symbols, denoting predicates and terms, and then a set of inference rules, called *schemes*, using this language.

The language is represented as a map from symbols to predicates and individuals, in the Clojure programming language. Each symbol of the language is mapped to a structure with fields for the symbol of the predicate (redundantly), the arity of the predicate (i.e. the number of columns in a tabular representation of the relation denoted by the predicate) and an optional number of forms for expressing statements and questions about this predicate in one or more natural languages.

To illustrate, below are the definition of some of the predicates of the language used in the D5.2 version of the schemes for practical reasoning. 

~~~{.clojure }
<#include "code/language.clj">
~~~	   
 
An argumentation scheme is represented as a structure with six fields:

1. id, 2. header, 3. conclusion 4. premises, 5. exceptions, and 6. assumptions. The id is a term in the language used to reify argumentation schemes and represent statements about argumentation schemes in domain models. The header enables metadata about the scheme to be represented (e.g. title, description). Descriptions can be represented in multiple natural languages. The conclusion is a formula schema, which may contain schema variables. Schema variables are represented by symbols beginning with a question mark, e.g `?Ag`, `?A`, and `?G`, and range over both terms and propositions. Thus, the conclusion of an argumentation scheme can be a schema variable. This feature is needed for representing schemes, such as arguments from expert witness testimony, whose conclusion may be any proposition whatsoever.

The premises, exceptions and assumptions fields of schemes are vectors of premise structures, where each premise has the following properties:

role
:   A string naming the role of the premise in the argumentation scheme, e.g. "major", "minor", "circumstances", "goal".

positive
:   Boolean. False if the premise is negated. Default: true.

statement
:   A formula schema.

implicit
:   True if the premise was not explicitly stated in the natural     language text of the source document or documents. (Arguments with implicit premises are called "enthymemes".)

Next, using this language we formally define the positive and negative versions of a scheme for practical reasoning. Schemes may be organized in an hierarchy of *sections*, each section with its own metadata. But since there are only four schemes in this example, sections are not illustrated here.

~~~{.clojure }
<#include "code/schemes.clj">
~~~

Now, let's us package the language and schemes together in a theory. Conceptually, a theory is a set of propositions. Since the set may be infinite, theories are represented intensionally, as a set of axioms and strict and defeasible inference rules, called argumentation schemes.

A theory is modelled as a structure with the following fields:

header
:   A header with metadata (e.g. title, description) about the theory. Descriptions can be in multiple languages and can be arbitrarily long, structured texts, represented using the Markdown wiki language.

language
:   The formal language of the theory; a dictionary mapping symbols to terms and predicates.

schemes
:   Strict and defeasible inference rules.

sections
:   A sequence of sections, which in turn consist of a header, schemes and (sub)sections, enabling theories to be organized hierarchically, similar to the hierarchical structure of books and articles.

references
:   A sequence of metadata structures, for providing bibliographic     information about source documents.

Next, we complete this section, and our illustration of how to implement argumentation schemes, by defining `theory1` to be the following theory:

~~~{.clojure }
<#include "code/theory1.clj">
~~~

## Copyright Policy Example

## Installing Policy Models and Argumentation Schemes 
