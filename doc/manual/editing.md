
# Editing Argument Graphs

To edit an argument graph, first go to the argument graph page, as described in the Chapter entitled [Browsing, Visualizing and Evaluating Arguments](#browsing-visualizing-evaluating-arguments).

![An Argument Graph Page](figs/graph1.png)

You can add nodes to the graph by clicking on the "New Statement" or "New Argument" buttons in the menu bar. In both cases you will be presented with a form to enter the required information. The form will be inserted and displayed at the top of the current argument graph page, so that you can scroll down to view information about the argument graph, without having to toggle back and forth between two tabs or pages in your web browser. See the [Entering New Statements](#entering-new-statements) or [Entering New Arguments](#entering-new-arguments) sections for further information.

To add a new argument pro or con some existing statement, go to the statement page and click on the "New Argument" button in its menu bar. The conclusion of the new argument will be set to the existing statement. Then complete the rest of the form as described in the [Entering New Arguments](#entering-new-arguments) section.

To edit or delete existing statements or arguments, first go to the page of the statement or argument and then click on the "Edit" or "Delete" button of its menu bar. Editing statements and arguments is done using the same forms used to create new statements and arguments. Deleting a statement will also delete the arguments pro or con the statement. Deleting an argument does not delete the conclusion or premises of the argument. This can leave some statements in the argument graph being unused in any argument.  

**Warning**: There is undo function, so all editing and delete operations are permanent. However, you will be asked to confirm all delete operations and have the option of cancelling or saving editing operations.

## Reconstructing Arguments

To reconstruct a *new* argument in some source text, one which is not already in the argument graph, follow the procedure below. If instead the source text is *another formulation* of an argument already in the argument graph, you can modify the description of the existing argument to also quote this source.

1. Optional: If the source text is not already in the list of references of the argument graph, you can add it by following the instructions in the [Editing the Metadata and Reference List of an Argument Graph](#editing-the-metadata-and-reference-list-of-an-argument-graph) section of this manual. When doing so you will assign the source document a "key" (label). Remember or copy this key.
2. Click on the "New Argument" button in menu bar of the argument graph page.
3. Copy the text of the argument from the source document and paste it into the description field of the new argument form. You can quote the text of the argument, using [Markdown](http://daringfireball.net/projects/markdown/), by preceding each line with a ">" symbol. At the end of the quotation, cite the source document, using the key you assigned it when adding it to the list of references, using the [Pandoc extension of Markdown for citations](http://johnmacfarlane.net/pandoc/README.html#automatic-citations). For example, if the key is "BenchCapon:2011" the cite would have the form `[@BenchCapon:2011, pg. 5]`. The page number or numbers are optional. See the [Pandoc](http://johnmacfarlane.net/pandoc/README.html#automatic-citations) documentation for details.
4. Optional: You can choose an argumentation scheme to apply, from the pull down list of schemes in the argument editor. If you choose a scheme, the form will be customized for the scheme, with fields intialized for each of its premises, exceptions and assumptions. You can modify the argument however you want, unconstrained by the chosen scheme, for example by deleting or adding premises, or renaming premise roles. The schemes are there to help you, not constrain you. (You can however check whether the argument complies with the given scheme. See the [Validating Arguments](#vaidating-arguments) section for details.) 
5. Enter the conclusion of the argument, either by choosing a statement already in the graph with the same meaning as the conclusion of the argument in the source text or by creating a new statement, by clicking on the plus (+) button to the right of the "select a statement" message. (Move the move over the "select a statement" message to see the plus sign, which is otherwise hidden.) If you create a new statement, you can either quote the conclusion of the argument in the "text" field of this new statement, or reformulate the conclusion in your own words.
6. Similarly, add the premises of the argument, either by choosing existing statements in the argument graph or by adding new statements. 
7. At the bottom of the form, click the "Save Argument" or "Cancel" button. **Warning:** Any changes you make using the form will not be saved to the argument graph until you execute the "Save Argument" command by clicking on this button. 

## Editing the Metadata and Reference List of an Argument Graph

(*Note: Not yet implemented.*)

## Entering New Statements

The form for entering new statements is shown when you click on the "New Statement" button in the menu bar of the argument graph page or on a plus sign (+) next to the conclusion or premise pull-down selection boxes for statements.  (The plus sign allows you to enter a new statement if the statement you need is not listed in the pull-down menu.)

![New Statement Form](figs/editor1.png)

You can use the *description* field to provide whatever information you want about the argument. The description can include headers,  lists, quotations, hypertext links and other elements, using the [Markdown](http://daringfireball.net/projects/markdown/) wiki language. The form includes a Markdown editor to make this easier for you. 

### Statement Description

You can enter translations of the description. There is a tab of the description form for each available language. (The set of languages can be configured by system administrators when installing the system. *Note: not yet implemented.*) The system is preconfigured for the following languages:

En
:   English
De
:   German
Fr
:   French
It
:   Italian
Nl
:   Dutch
Sp
:   Spanish


### Metadata Elements

Statements can be annotated with metadata properties, using the [Dublin Core](http://en.wikipedia.org/wiki/Dublin_Core) metadata elements.  The Dublin Core elements are briefly summarized in the [Metadata](#metadata) section of this manual.

To add a metadata element to a statement, select the element from the pull-down menu and click on the "Add" button. A text box for entering the value of the element will be added to the form. Multiple values of an element can be added by clicking on the "Add" button as often as needed. A value can be deleted by clicking on the "x" icon to the right of the value.


### Atoms: Formalizing Statements using Predicate Logic

Optionally, statements can be formalized in predicate logic, by providing a value for the "atom" property in the form. This is an advanced feature that may be needed only for more specialized application scenarios.

Atoms are formalized in Carneades using the prefix notation of [s-expressions](http://en.wikipedia.org/wiki/S-expression) ("symbolic expressions"), as in the Lisp family of programming languages.

For example, Socrates is a man could be formalized as `(man Socrates)`. Here, `man` is a unary predicate symbol.  The fact that Socrates died in 399 B.C. could be represented using a binary predicate, for example as `(died Socrates -399)`.  

Atoms are not limited to unary or binary predicates. Atoms with an   arity of zero or more than 2 are also allowed.

When argumentation schemes are used to enter new arguments, the statements of the argument are initialized with atom schemas.  You can either delete the atom, if they are not needed for your application scenario, or replace the variables in the atom scheme with constants. 


### Statement Text

### Proof Standard

### Main Issue

### Statement Weights

	
## Editing Statements
## Deleting Statements
## Entering New Arguments
## Editing Arguments
## Validating Arguments
## Deleting Arguments