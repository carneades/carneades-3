
#  Modeling Policies and Argumentation Schemes

Argumentation schemes generalize the notion of an inference rule to
cover defeasible as well as strict reasoning patterns.  Carneades
provides a high-level programming language for representing
argumentation schemes and "deep conceptual" or "semantic" models of
domains. This language is based-on a higher-order predicate logic
expressive enough to represent axiomatizations (i.e. axioms and
inferences rules) of theories of any domain, including policies.

The Carneades scheme language is semantically similar to Prolog, with
some extensions required for representing argumentation schemes (i.e.
strict and defeasible inference rules), along with meta-data and
documentation, in multiple languages, and for organizing schemes in an
hierarchical outline, to facilitate "isomorphic modeling" of source
documents, such as legislation.

The language for argumentation schemes is an executable
knowledge-representation language, not an "interchange format". No
database is used for representing or storing argumentation schemes.
(Carneades does provide a relational database for storing the
arguments which instantiate these schemes.) Rather, the argumentation
schemes are represented in text files, using the scheme language. The
Carneades system provides an inference engine for this language which
is able to fully automatically construct arguments from schemes and
"deep conceptual" or "semantic" models represented in this
language. Using a high-level declarative rule language for
representing argumentation schemes and semantics models, and
generating arguments from these schemes and models, makes it easy for
domain experts to extend the model to support further argumentation
schemes, without requiring knowledge about relational database schemes
or the implementation of middleware.

In this section the language is illustrated by reconstructing the
argumentation schemes presented in Version 0.5 of Deliverable D5.2.[^2]
Other argumentation schemes, or alternative formulations of these
schemes, can be represented in a similar manner.

The semantics of argumentation schemes in this language is formally
defined in Section "[Generating Arguments from Semantic Models](#generating-arguments-from-semantic-models)".
Briefly, instantiations of argumentation schemes are mapped to argument
graphs. Argument graphs, in turn, are mapped to Dung abstract
argumentation frameworks. Thus, argumentation schemes, in this formal
semantics, are abstractions of Dung argumentation frameworks.

A domain theory is represented by first defining a *language*
(dictionary) of symbols, denoting predicates and terms, and then a set
of inference rules, called *schemes*, using this language.

The language is represented as a map from symbols to predicates and
individuals, in the Clojure programming language. Each symbol of the
language is mapped to a structure with fields for the symbol of the
predicate (redundantly), the arity of the predicate (i.e. the number of
columns in a tabular representation of the relation denoted by the
predicate) and an optional number of forms for expressing statements and
questions about this predicate in one or more natural languages.

To illustrate, below are the definition of some of the predicates of the
language used in the D5.2 version of the schemes for practical
reasoning. 

~~~{.clojure }
<#include "code/language.clj">
~~~	   
 
An argumentation scheme is represented as a structure with six fields:
1. id, 2. header, 3. conclusion 4. premises, 5. exceptions, and 6.
assumptions. The id is a term in the language used to reify
argumentation schemes and represent statements about argumentation
schemes in domain models. The header enables metadata about the scheme
to be represented (e.g. title, description). Descriptions can be
represented in multiple natural languages. The conclusion is a formula
schema, which may contain schema variables. Schema variables are
represented by symbols beginning with a question mark, e.g `?Ag`,
`?A`, and `?G`, and range over both terms and propositions. Thus,
the conclusion of an argumentation scheme can be a schema variable. This
feature is needed for representing schemes, such as arguments from
expert witness testimony, whose conclusion may be any proposition
whatsoever.

The premises, exceptions and assumptions fields of schemes are vectors
of premise structures, where each premise has the following properties:

role
:   A string naming the role of the premise in the argumentation scheme,
    e.g. "major", "minor", "circumstances", "goal".

positive
:   Boolean. False if the premise is negated. Default: true.

statement
:   A formula schema.

implicit
:   True if the premise was not explicitly stated in the natural
    language text of the source document or documents. (Arguments with
    implicit premises are called "enthymemes".)

Next, using this language we formally define the positive and negative
versions of the scheme for practical reasoning from the draft of D5.2.
Schemes may be organized in an hierarchy of *sections*, each section
with its own metadata. Since there are only four schemes in this
example, sections are not illustrated here.

~~~{.clojure }
<#include "code/schemes.clj">
~~~

Now, let's us package the language and schemes together in a theory.
Conceptually, a theory is a set of propositions. Since the set may be
infinite, theories are represented intensionally, as a set of axioms and
strict and defeasible inference rules, called argumentation schemes.

A theory is modelled as a structure with the following fields:

header
:   A header with metadata (e.g. title, description) about the theory.
    Descriptions can be in multiple languages and can be arbitrarily
    long, structured texts, represented using the Markdown wiki
    language.

language
:   The formal language of the theory; a dictionary mapping symbols to
    terms and predicates.

schemes
:   Strict and defeasible inference rules.

sections
:   A sequence of sections, which in turn consist of a header, schemes
    and (sub)sections, enabling theories to be organized hierarchically,
    similar to the hierarchical structure of books and articles.

references
:   A sequence of metadata structures, for providing bibliographic
    information about source documents.

Next, we complete this section, and our illustration of how to implement
argumentation schemes, by defining `liverpool-schemes` to be the
following theory:

~~~{.clojure }
<#include "code/liverpool-schemes.clj">
~~~