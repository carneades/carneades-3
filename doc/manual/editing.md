
# Editing Argument Graphs

To edit an argument graph, first go to its outline, as described in
the Chapter entitled
[Browsing, Visualizing and Evaluating Arguments](#browsing-visualizing-evaluating-arguments).
An example argument graph page is shown in the figure below.

![An Argument Graph Page](figs/graph1.png)

You can add nodes to the graph by clicking on the "New Statement" or
"New Argument" buttons. In both cases you will be presented with a
form to enter the required information. The form will be displayed in
a new tab, so that you can toggle back and forth between different
views of the argument graph will editing.  (**Warning**: A known bug
in the user interface causes your work to be lost if you change to
another tab without first saving your work.) See the
[Entering New Statements](#entering-new-statements) or
[Entering New Arguments](#entering-new-arguments) sections for further
information.

To add a new argument pro or con some existing statement, go to the
statement page and click on the "New Argument" button. The conclusion
of the new argument will be set to the existing statement. Then
complete the rest of the form as described in the
[Entering New Arguments](#entering-new-arguments) section.

To edit or delete existing statements or arguments, first go to the
page of the statement or argument and then click on the "Edit" or
"Remove" button. Editing statements and arguments is
done using the same forms used to create new statements and
arguments. Deleting a statement will also delete the arguments pro or
con the statement. Deleting an argument does not delete the conclusion
or premises of the argument. This can leave some statements in the
argument graph being unused in any argument.

**Warning**: There is no undo function, so all editing and delete
operations are permanent. However, you will be asked to confirm all
delete operations and have the option of cancelling or saving
editing operations.

## Reconstructing Arguments

To reconstruct a *new* argument in some source text, one which is not
already in the argument graph, follow the procedure below. If instead
the source text is *another formulation* of an argument already in the
argument graph, you can modify the description of the existing
argument to also quote this as an additional source of the argument.

1. Click on the "New Argument" button on the outline 
   page of the argument graph.
2. Copy the text of the argument from the source document and paste it
   into the description field of the "Metadata"tab of new argument
   form. You can quote the text of the argument, using
   [Markdown](http://daringfireball.net/projects/markdown/), by
   preceding each line with a ">" symbol.
3. Go the "Model" tab of argument editor and choose an argumentation
   scheme to apply, from the pull down list of schemes. The
   documentation of the selected scheme will be shown and the form
   will be customized, with fields intialized for each of the premises
   of the scheme. You can modify the argument however you want,
   unconstrained by the chosen scheme, for example by deleting or
   adding premises, or renaming premise roles. The schemes are there
   to help you, not constrain you. See the
   [Argumentation Schemes](#argumentation-schemes) chapter for
   documentation of the initial set of schemes provided with Carneades. These
   schemes may be modified or extended, or replaced entirely, as
   described in the [Argumentation Schemes](#argumentation-schemes) chapter.
4. Enter the conclusion of the argument, by choosing a statement
   already in the graph with the same meaning as the conclusion of the
   argument in the source text. If the needed statement is not listed,
   create one first following the instructions for entering new
   statements below. 
5. Similarly, add the premises of the argument, by choosing existing
   statements in the argument graph from the pull-down lists below the
   role of each premise in the form. Negative premises can be entered
   by changing the "Positive" property of the premise to "False". If
   the premise is implicit in the source text, you can note this by
   changing the "Implicit" property of the premise to "True".
6. At the bottom of the form, click the "Save Argument" or "Cancel"
   button. **Warning:** Any changes you make using the form will not
   be saved to the argument graph until you execute the "Save
   Argument" command by clicking on this button.

## Entering New Statements

The form for entering new statements is shown when you click on the
"New Statement" button on the outline page of the argument graph page.

![New Statement Form](figs/editor1.png)

A new statement should not be entered into the argument graph if the
negation of the statement is already in the argument graph.  That is,
for every proposition $P$, a single statement node should be entered
into the graph to represent *both* $P$ and $\neg P$. It doesn't matter
whether the positive or negative form of the statement is included
explictly in the argument graph. Typically the positive form is used,
but in some cases you may prefer the negative form.  For example, you
may prefer to include the sentence "The payment was illegal"
explicitly in the argument graph and then represent arguments pro the
legality of the payment as argument con the claim the payment was
illegal. The best choice may depend on the procedural context of the
debate.

Only one form of the statement is required, since arguments pro the
negation of the statement are equivalent to arguments con the
statement. And premises of arguments can be explicitly negated in
argument graphs, and in the forms for entering and editing
arguments. This approach has the advantage of reducing the number of
statements in the graph up to 50%.

The new statement form is divided into two sections, named "Model" and
"Metadata". On smaller screens these two sections are shown in
separate tabs, labelled accordingly.  The "model" consists of the text
of the statement, in one or more languages, along with some additional
properties, described in detail below. The "metadata" consists of
generic properties, not specific to statements, such as a title and
description. Be careful not to confuse the description of the
statement with the text of the statement. The text is a formulation of
the statement, whereas as the description provides further information
about the statement.

### Main Issue

An argument graph should have at least one main issue.  These are the
issues which are central to the debate.  All the other issues are
subsidiary issues only important to the extent that they are relevant
for resolving one of the main issues.

### Statement Text

The "text" property of a statement node of an argument graph is for
expressing the statement concisely in natural language. You should
always provide such a text.  As for descriptions, translations of the
text in several languages may be included in the model and the
statment may be structured using
[Markdown](http://daringfireball.net/projects/markdown/). This is the
text that will appear in statement boxes in argument maps and in
hypertext views of the argument graph. Whereas descriptions are
optional and part of the *metadata* about the statement, this text is
the content representing the statement itself.

Translations of the text can be entered in several natural languages:

En
  : English

De
  : German

Fr
  : French

It
  : Italian

Nl
  : Dutch

Sp
  : Spanish

### Atom: Formalizing Statements using Predicate Logic

Optionally, statements can be formalized in predicate logic, by
providing a value for the "atom" property in the form. This is an
advanced feature that may be needed only for more specialized
application scenarios.

Atoms are formalized in Carneades using the prefix notation of
[s-expressions](http://en.wikipedia.org/wiki/S-expression) ("symbolic
expressions"), as in the Lisp family of programming languages.

For example, Socrates is a man could be formalized as `(man Socrates)`. Here, `man` is a unary predicate symbol.  The fact that Socrates died in 399 B.C. could be represented using a binary predicate, for example as `(died Socrates -399)`.  

The language for Atoms is quite expressive:
	
- The arity of atoms can be 0 or more, as in predicate logic, not
  restricted to unary and binary predicates, as in description logic,
  RDF and OWL.  Examples: `(good)`, `(between 0 1 2)`.
- Compound terms may be used.  For example `(= 2 (+ 1 1))`. The is,
  the atom language is not restricted to Datalog.
- Atoms can be higher-order.  Example: `(believes Gerald (and (made-of
  Moon green-cheese) (really-exists Yeti)))`.  Notice that compound
  propositions can be reified as terms, as in this example.

### Proof Standard

The proof standard of a statement determines how much proof is
required for the statement to be deemed acceptable (presumably
true). The proof standard is used by the computational model to
argument to compute the acceptability of the statement. Several proof
standards are available:

Dialectical Validity (DV)
:   This standard is the only one that does not make use of arugment
    weights. It is satisfied if at least one pro argument is *in* and no
    con argument is *in*.

Preponderance of Evidence (PE)
:   This standard is met if at least one pro argument is *in* that
    weighs more than any *in* con argument. 

Clear and Convincing Evidence (CE)
:   This standard is satisfied if the preponderance of evidence standard
    is met and, in additional, the difference between the strongest
    *in* pro argument and the strongest *in* con argument is above a
    certain threshold.

Beyond Reasonable Doubt (BRD)
:   This standard is met if the clear and convincing evidence standard
    is meet and, in addition, the weight of the weakest *in* con argument
    is below a certain threshold.

The default proof standard is preponderance of the evidence, and for
most applications this proof standard should be sufficient. Note that
the preponderance of evidence standard is met whenever the
dialectical validity standard is met. If arguments are not weighed,
the dialectical validity and preponderance of evidence standards will
give the same results.  The preponderance of evidence, clear and
convincing evidence and beyond reasonable doubt standards are ordered
by the amount of proof required, with beyond reasonable doubt
requiring the most proof. Whenever one of these standards is met, all of
weaker standards are also meet.

### Statement Weight

The weight of a statement encodes the extent to which users agree or
disagree with the statement, on a scale of 0.0 to 1.0, where 0.0
represents disagree (reject), 0.5 means no opinion and to 1.0 is
denotes agree (accept). Statements with a weight greater then or equal
to 0.75 and less than 1.0 are *assumed* to be true, until an argument
pro or con the statement has been added to the argument
graph. Conversely, statements with a weight less than or equal to 0.5
and greater than 0.0 are assumed to be false, until an argument pro or
con the statement has been added to the graph.

### Statement Metadata

Statements can be annotated with metadata properties, using the
[Dublin Core](http://en.wikipedia.org/wiki/Dublin_Core) metadata
elements.  The Dublin Core elements are briefly summarized in the
[Metadata](#metadata) section of this manual.

To add metadata to a statement, go to the "Metadata" tab of the
statement editor and complete the form. Multiple values of an element
can be entered by listing. Separate the items in the list
with semicolon (";") characters. 

You can use the *description* field of the "Metadata" tab of the
statement editor to provide whatever background information you want
about the statement. You can structure and format the description,
including headers, lists, quotations, hypertext links and other
elements, using the
[Markdown](http://daringfireball.net/projects/markdown/) wiki
language. The form includes a Markdown editor to make this easier for
you.

As for the statement text, translations of descriptions can be entered
in several natural languages.
	
## Editing Statements

To edit an existing statement in an argument graph, first navigate to
the statement page and click "Edit Statement" button.  This will
reveal a form for modifying the statement.  This is
the same form used to enter new statements, described in the
[Entering New Statements](#entering-new-statements) section of this
chapter, but with the fields of the form filled in with the current
values of the properties of the statement. After making changes, click
on the "Save" button at the bottom of the form, to have the
changes stored in the argument graph database, or the "Cancel" button
to abort the editing process and retain the prior values of the
properties of the statement.

**Warning**: The changes cannot be undone after saving them.

## Deleting Statements

To delete a statement from an argument graph, first navigate to the
statement page and click of the "Remove Statement" button.  You will
be asked to confirm or cancel the deletion.

**Warning**: The deletion cannot be undone after it has been
confirmed. Deleting a statement also deletes all arguments pro or con
this statement, i.e. with this statement as the conclusion of the
argument. The statements for the premises of these arguments are not
deleted.

## Entering New Arguments

The form for entering new arguments is shown when you click on the
"New Argument" button in the menu bar of the argument graph page or a
statement page.  When used on a statement page, the form for entering
the new argument will be initialized with the statement as the
conclusion of the argument.

![New Argument Form](figs/editor2.png)

See the section on
[Reconstructing Arguments](#reconstructing-arguments) for tips about
how to proceed when interpreting and modeling arguments in source
texts.

### Using Argumentation Schemes

Arguments can be entered into the system, without using argumentation
schemes. Their use is entirely optional. That said, argumentation
schemes can be helpful for interpreting source texts when trying to
reconstruct arguments. They serve as templates which can guide you in
your task of understanding the source text. For example, if the
argument looks like it might be an argument from expert opinion, using
the expert opinion scheme will help you to remember the conventional
form of this kind of argument. Can the text reasonably be understood
as including all the premises of the expert witness scheme? Were any
missing premises left implicit by the author, or would this be reading
too much between the lines? Does the argument perhaps better fit some
other scheme?

See the section on
[Reconstructing Arguments](#reconstructing-arguments) for tips about
how to proceed when interpreting and modeling arguments in source
texts.

You can view a list of the available argumentation schemes using the
pull-down menu. Type in a string, such as "expert", to display the
schemes containing the string in the title. Then select the desired
scheme from the list. This will display a description of the scheme,
perhaps with example arguments, along with references to publications
about the scheme. You can go back and select another scheme at any
time.

Whenever a scheme is selected, the form will customized to include
premises and exceptions fields for the chosen scheme. The roles of the
premises and exceptions will be modified to match the selected
scheme. The premise roles can be edited. You are not constrained by
the scheme.

If you change your mind and select another scheme, any statements you
have selected for the conclusion and premises of the argument will
remain, but you will need to check whether they are still
appropriate and add, modifiy or delete premises as necessary.

Again, the argumentation schemes are meant to be helpful, not get in
your way. They do not constrain you to enter arguments matching the
schemes.  You can, however, check whether arguments correctly
instantiate the schemes they have been assigned. See the
[Validating Arguments](#validating-arguments) section for further
information.

### Entering and Deleting Premises

In the premise of the argument editor, you can choose an existing
statement in the argument graph from the pull down list.  You can
filter the statements in the list by typing some text in the text
field at the top of the list. 

You can insert additional premises by clicking on the "Add Premise"
button.  Premises can be deleted by clicking on the "Remove this
premise" button immediately below each premise.

### Strict or Defeasible Arguments

An argument may be *strict* or *defeasible* (not strict).  An argument
is strict if and only if its conclusion must be true, with no
exceptions, if its premises are true, whether or not its premises are
in fact true.  For example the following arguments are strict:

- The moon is made of green cheese since the moon is made of green
  cheese and unicorns have horns.
- The figure is a triangle since it has three sides.

The first example argument is of course cyclic, not to mention fanciful. But it is strict nonetheless.

Arguments are defeasible if they are not strict. With defeasible
arguments, the conclusion is only *presumably* true when the premises
are true. The argument gives us a reason to accept the conclusion, but
there may be exceptions or other reasons leading to the opposite
conclusion.  Here are a couple of famous examples from the
computational models of argument literature:

- The object is red since it looks red.
- Tweety flies sinces Tweety is a bird.

The object may look red because, for example, if is being illuminated
by a red light. And Tweety the bird may be a penguin, or have a broken
wing, and so on.

The conclusion of a defeasible argument need not be *probably* true,
in some statistical sense.  It is not necessary to have good empirical
data allowing us to draw conclusions about what is probably the case
in order to make a defeasible argument.  The argument only needs to
give us some good reason to believe the conclusion, at least until we
have heard arguments giving us reasons to the contrary.

Select whether the argument is strict in the form using the radio
buttons. By default, arguments are defeasible.

### Argument Direction

Arguments can be *pro* or *con* their conclusion.  An argument con a
statement is semantically equivalent to an argument pro the negation
of the statement. 

### Argument Weight

Argument weights only become important when two or more arguments with
contradictory conclusions, that is arguments pro and con some
conclusion, come into play. Such arguments are called "rebuttals".
The conflict between the rebuttals is resolved by weighing the
arguments and applying proof standards. See the
[Proof Standard](#proof-standard) of this chapter for further
information.

The weight can be entered directly in the argument editor, using the
slider. 


### Argument Metadata

Arguments can be annotated with metadata properties, using the
[Dublin Core](http://en.wikipedia.org/wiki/Dublin_Core) metadata
elements, in the same way as statements. See the
[Statement Metadata](#statement-metadata) section for further
instructions.

You can use the *description* field of the argument to provide
whatever background information you want about the argument, including
quotations of formulations of the argument in source texts, along with
hyperlinks to the sources.

You have the same possibilities for structuring the text of the
description, using the
[Markdown](http://daringfireball.net/projects/markdown/) wiki
language, and entering translations of the description as you have for
statement descriptions. See the
[Statement Description](#statement-description) section of this
chapter for further information.

## Editing Arguments

To edit an existing argument in an argument graph, first navigate to
the argument page and then click on the "Edit Argument" button. This
will open a new tab with an editor for modifying the argument.
This is the same form used to enter new arguments, described in the
[Entering New Arguments](#entering-new-arguments) section of this
chapter, but with the fields of the form filled in with the current
values of the properties of the argument. After making changes, click
on the "Save" button at the bottom of the form, to have the
changes stored in the argument graph database, or the "Cancel" button
to abort the editing process and retain the prior values of the
properties of the statement.

**Warning**: The changes cannot be undone after saving them.

<!--
## Validating Arguments

(*Note: not yet implemented*)

By argument "validation" here we mean checking whether an argument
correctly instantiates the argumentation scheme it has been assigned,
if any. (The validation process does *not* check whether the premises
or conclusion of the argument are true or acceptable.) The argument
passes this validity test if it matches its scheme.  If it does not
match the scheme, a report is displayed showing how the argument fails
to match the scheme.

Arguments can be validated at your choice of different levels of granularity. From the least to the most fine-grained, these levels are:

1. The argument matches the scheme if and only if it has the same number of premises as the scheme, and these premises also have the same "role" labels. 
2. In addition to the above test, the text of the statements in the conclusion and premises of the argument match the statements in the conclusion and premises of the scheme, for each natural language representation of the text in the argument.  The natural language templates defined in the scheme are used to test the matches, by trying to replace variables in the templates with substrings in the text of the statements in the argument.
3. In addition to the above test, the atoms (predicate logic formulas) of the conclusion and premises of the argument match the atoms of the scheme.

Exceptions in argumentation schemes are represented as undercutting arguments in argument graphs and are validated a bit differently. Each exception of an argumentation scheme is used to construct a separate undercutter argument, with a single premise matching the exception in the scheme. Thus, an undercutter is valid if its premise matches one of the exceptions of the scheme.

Undercutters are not validating automatically when validating the argument they undercut.

To validate an argument, go to the argument page and click on the "Validate" button in the menu bar. You will be asked to select the level of granularity to check. The command will then generate and display a validation report either confirming that the argument is valid or listing all the mismatches so that you can repair them using the [argument editing](#editing-arguments) tool.

You can also validate all of the arguments in the argument graph, in one step, by going to the argument graph page and clicking on the "validate" button of its menu bar.
-->

## Deleting Arguments

To delete an argument from an argument graph, first navigate to the
argument page and then click of the "Remove Argument" button.  You
will be asked to confirm or cancel the deletion.

Deleting an the argument does not delete the statements used in its
conclusion or premises.

Any arguments undercutting this argument will also be deleted, since
the conclusion of such an undercutter refers to this argument
specifically and can serve no function once the argument has been
deleted.

**Warning**: The deletion cannot be undone after it has been confirmed. 

<!--
## Using Polls to Weigh Statements and Arguments

Public users can be polled for their opinions and the results of the poll can be averaged to compute the weights of statements and arguments. See the [Formulating, Polling and Comparing Opinions](#formulating-polling-and-comparing-opinions) chapter for further information about polls. 

To compute the weights of statements and arguments from the poll results, go to the argument graph page and click on the "Weigh" button in the menu bar. (*Note: not yet implemented.*)  Afterwards you may also want to re-evaluate the argument graph to update the acceptability labels of the statement and argument nodes, as explained in the [Evaluating Argument Graphs](#evaluating-argument-graphs) section.
-->

##  Evaluating Argument Graphs

The term "argument evaluation" has two different but related meanings.
The broader meaning, described in the
[Evaluating Arguments](#evaluating-arguments) section of this manual,
concerns the process of critically assessing the validity of
arguments, by

1. revealing implicit premises
2. validating whether the arguments are formally correct, by
   instantiating accepted argumentation schemes
3. asking appropriate critical questions, depending on the schemes applied
4. and determining which claims are acceptable, taking into
   consideration the assumptions of the users and their collective
   assessment of the relative weights of conflicting pro and con
   arguments.

The final step of this process, determining which claims are
acceptable, is what we mean by argument evaluation in this
section. Perhaps a better term for this narrower conception of
argument evaluation, to avoid confusion, would be "argument labeling",
since the result of this kind of argument evaluation is to label the
statement and argument nodes of an argument graph as being "in", "out"
or "undecided", where:

in
:   means the statement is *acceptable* (presumably true) or the
    argument is *applicable*, because all of its premises are in and it
    has not been undercut by an in argument.

out
:   means the negation of the statement is acceptable (equivalently: the
    statement itself is not acceptable) or the argument is not
    applicable.

undecided
:   means the statement or argument is neither in nor out.

Argument graphs are evaluated using the computational model of
structured argument presented in [@GordonPrakkenWalton:2007a].  The
argument graph is automatically re-evaluated, updating the labels of
the argument and statement nodes, whenever the argument graph is
modified. This includes not only changes to the weights of the
statements and arguments, but also changes to the assigned proof
standards and, the addition or deletion of any arguments. (Additional
arguments, not only deletions, can cause the labels to change, since
acceptability is a *nonmonotonic* inference relation.)

<!--
Argument graphs are labelled by mapping the graph to a Dung abstract
argumentation framework [@Dung1995], using the ASPIC+ model of
structured argument [@Prakken:2010a] in a way which preserves the
features of the original version of Carneades, in particular its
support for proof standards [@GordonPrakkenWalton:2007a]. Carneades
uses grounded semantics for the resulting Dung argumentation
framework, which is the most cautious (skeptical) semantics with
regard to the claims it finds acceptable.
-->

<!--
## Importing XML

(*Note: Not yet implemented.*)

The Carneades Argument Format (CAF) is an XML schema for storing
argument graphs in files, in a structured but plain text form which
can be read or edited by humans, using any text editor, that is also
suitable for machine processing.

To facilitate moving statements and arguments between argument graphs,
across installations of the Carneades system, every statement and
argument is assigned an
[Uniform Resource Name](http://en.wikipedia.org/wiki/Uniform_resource_name)
(URN).  URNs are identifiers which are extremely likely to be unique,
world-wide.

(*Note: Write a reference manual describing the CAF format.*) 

For information about how to *export* argument graphs to XML, see the
[Exporting Argument Graphs to XML](#exporting-argument-graphs-to-XML)
section of this user manual.

To import a CAF file, navigate to the argument graph page and click on the "Import" button in the menu bar. You be asked to select a file from your file system.  The selected filed will be validated to make sure that it is a valid CAF file. It if it is valid, it will be used to update the argument graph, as follows.  

1. First, you will be asked if you want to *merge* or *overwrite* the
   data in the argument graph in the database with the data in the CAF
   file.
2. If you choose to *merge* the data:
	- The properties of statements or arguments in the argument graph
      will be *replaced* by the values of the same properties in the
      CAF file but the values of properties undefined in the CAF file
      will retain their current values in the database. (**Note**: all
      values of a multivalued metadata property will be *replaced* by
      the values of the property in the CAF file. The values of
      individual properties are not merged.)
	- Any new statements or arguments in the CAF file will be added to
      the database, without deleting statements or arguments in the
      database which are not in the CAF file.
	- The metadata of the argument graph as a whole will be updated
      using the metadata in the CAF file. Properties of the argument
      graph will be replaced by the values of the same properties in
      the CAF file. (**Note**: If a property has multipled values,
      they will all be replaced, not merged.) Properties not in the
      CAF file will retain their current values in the database.
	- References of the argument graph in the database will be
      replaced by references with the same keys in the CAF file, but
      references in the database which are not in the CAF file will be
      retained, not deleted.
3. If instead you choose to *overwrite* the data:
	- The properties of statements or arguments in the argument graph
      will be *replaced* by the values of the same properties in the
      CAF file and properties undefined in the CAF file will be
      deleted in the database. (**Note**: all values of a multivalued
      metadata property will be *replaced* by the values of the
      property in the CAF file. The values of individual properties
      are not merged.)
	- Any new statements or arguments in the CAF file will be added to
      the database, without deleting statements or arguments in the
      database which are not in the CAF file.
	- The metadata of the argument graph as a whole will be updated
      using the metadata in the CAF file. Properties of the argument
      graph will be replaced by the values of the same properties in
      the CAF file. (**Note**: If a property has multipled values,
      they will all be replaced, not merged.) Properties not in the
      CAF file will be deleted in the database.
	- References of the argument graph in the database will be
      replaced by references with the same keys in the CAF file and
      references in the database which are not in the CAF file will be
      deleted.
-->




