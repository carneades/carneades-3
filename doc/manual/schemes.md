

# Argumentation Schemes

Argumentation schemes are represented using a high-level scheme (rule)
language embedded in the Clojure programming language.

Argumentation schemes generalize the notion of an inference rule to
cover defeasible as well as strict reasoning patterns. We use the term
"scheme" instead of "rule" to emphasize that the rules are usually
defeasible. The scheme language is expressive enough to represent
axiomatizations (i.e. axioms and inferences rules) of theories in many
domains, including laws, regulations and policies, in addition to
argumentation schemes per se.

Computational models of theories have been called many things in
computer science, including "knowledge bases" and "deep conceptual" or
"semantic" models. We prefer the term "theory" to "knowledge-base"
because "knowledge" may suggest consensus or truth, while it is
clearer that theories may be controversial or contested.

The scheme language is similar to logic programming languages such as
Prolog. Any Prolog rule ("clause") can be represented in Carneades in
a straight-forward way. The rule language has some additional features
for representing argumentation schemes, such as scheme variables
ranging over atoms and a means to represent premise roles
(e.g. "major", "minor"). Moreover, schemes in this language can be
annotated with meta-data and documentation, in multiple natural
languages.

Carneades includes an inference engine which is able to automatically
apply theories to construct arguments. Using a high-level declarative
language for representing theories, and generating arguments from
these theories, makes it easier for domain experts to read and
validate the theories.

Theories may organized in a hierarchical structure of sections and
subsections, with schemes included at any level. Like schemes, the
theory as a whole and each of its sections can be annotated with its
own meta-data and natural language description. These features
facilitate self-documenting and "isomorphic" modeling. The source text
of the schemes, policies or legislation can be included within the
model, in the same files, in such as way as to preserve the
hierarchical structure of the sources. This makes it easier to
maintain the model as the source documents are modified, since there
can be a one-to-one correspondance between sections of the source text
and sections of the model.

Theories are represented in Clojure source code files.  Clojure
packages can be used to modularize, combine and reuse theories.  

The theory language is an executable knowledge-representation
language, with its own semantics and an inference engine implementing
this semantics.  It is not intended to be used as an "interchange
format" for exporting and importing theories among diverse formalisms
with varied semantics.

The semantics of theories are defined by mapping
instantiations of argumentation schemes to argument graphs and, in
turn, evaluating these graphs using the computational model of
structured argument presented in [@GordonPrakkenWalton:2007a].

## Modeling Argumentation Schemes 

In this section we illustrate how to use the language to represent a
version of the scheme for arguments from practical reasoning.

A domain theory is represented by first defining a *language*
(dictionary) of symbols, denoting predicates and terms, and then a set
of inference rules, called *schemes*, using this language.

The language is represented as a map from symbols to predicates and
individuals, in the Clojure programming language. Each symbol of the
language is mapped to a structure with fields for the symbol of the
predicate (redundantly), the arity of the predicate (i.e. the number
of columns in a tabular representation of the relation denoted by the
predicate) and an optional number of forms for expressing statements
and questions about this predicate in one or more natural languages.

To illustrate, below are the definition of some of the predicates of
the language used a version of the schemes for practical reasoning.

~~~{.clojure }
<#include "code/language.clj">
~~~	   
 
An argumentation scheme is represented as a structure with six fields:

1. id
2. header
3. conclusion
4. premises
5. exceptions, and
6. assumptions.

The id is a term in the language used to reify argumentation schemes
and represent statements about argumentation schemes in domain
models. The header enables metadata about the scheme to be represented
(e.g. title, description). Descriptions can be represented in multiple
natural languages. The conclusion is a formula schema, which may
contain schema variables. Schema variables are represented by symbols
beginning with a question mark, e.g `?Ag`, `?A`, and `?G`, and range
over both terms and propositions. Thus, the conclusion of an
argumentation scheme can be a schema variable. This feature is needed
for representing schemes, such as arguments from expert witness
testimony, whose conclusion may be any proposition whatsoever.

The premises, exceptions and assumptions fields of schemes are vectors
of premise structures, where each premise has the following
properties:

role
  : A string naming the role of the premise in the argumentation
  scheme, e.g. "major", "minor", "circumstances", "goal".

positive
  : Boolean. False if the premise is negated. Default: true.

statement
  : An atom formalizing the propositional content of the statement.

Next, using this language we formally define positive and negative
versions of a scheme for practical reasoning. Schemes may be organized
in an hierarchy of *sections*, each section with its own metadata. But
since there are only four schemes in this example, sections are not
illustrated here.

~~~{.clojure }
<#include "code/schemes.clj">
~~~

Now, let's us package the language and schemes together in a
theory. Conceptually, a theory is a set of propositions. But since the
set may be infinite, it is more convenient to represent theories
intensionally, as a set of axioms and inference rules. We call the
inference rules "argumentation schemes", because they may be defeasible,
to distinguish them from the inference rules of classical logic, which
are all non-defeasible (strict).

A theory is modelled as a structure having the following fields:

header
  : metadata (e.g. title, description) about the
  theory. Descriptions can be in multiple languages and can be
  arbitrarily long, structured texts, represented using the Markdown
  wiki language.

language
  : The formal language of the theory; a dictionary mapping symbols to
  terms and predicates.

schemes
  : Strict and defeasible inference rules.

sections
  : A sequence of sections, which in turn consist of a header, schemes
  and (sub)sections, enabling theories to be organized hierarchically,
  similar to the hierarchical structure of books and articles.

references
  : A sequence of metadata structures, for providing bibliographic
  information about source documents.

Next, we complete this illustration of how to implement argumentation
schemes, by defining `theory1` to be the following theory:

~~~{.clojure }
<#include "code/theory1.clj">
~~~

<!--
## Modeling Policies

Policies and argumentation schemes are modeled in the same way.  This
section illustrates how to use the language to model policies, with a
copyright example.

The policy issue in this example is whether so-called "orphaned" works
may be published without a license for some purposes.

We assume in this example that current copyright law in Germany
requires a license, with no exceptions for orphaned works. This is the
"status quo" policy.  An alternative policy is proposed by a German
non-profit organization, the Aktionsbündnisses "Urheberrecht für
Bildung und Wissenschaft", called the "Action Alliance" in the
following. This policy would allow orphaned works to be published,
with or without a license, under exceptional circumstances, for
example when the work is published for non-commercial purposes and an
effort has been made to search for the copyright owner.

We begin by modeling the technical language ("ontology") of the
policies:

~~~{.clojure }
<#include "code/copyright_language.clj">
~~~

This policy consists of a single scheme, modeling Section 31 of German
copyright law. The symbol `UrhG-31` has been defined to refer to this
scheme, so as to allow the scheme to be used in multiple policies,
since it used both by the current German law and the policy proposed
by the Action Alliance.

~~~{.clojure }
<#include "code/german_copyright_law.clj">
~~~

Next, here is the model of the alternative, more liberal policy
proposed by the Action Alliance:

~~~{.clojure }
<#include "code/action_alliance_policy.clj">
~~~

Notice how Section 31 of German copyright law is also a part of this
policy proposal. The proposed policy extends German law with
exceptions for orphaned works.

Now, let's put this altogether is a "theory" containing both policies:

~~~{.clojure }
<#include "code/copyright_policies.clj">
~~~

## Installing Policy Models and Argumentation Schemes 
-->

















