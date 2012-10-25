# Carneades Tasks and Notes .todo

- Update the implementation of the argumentation schemes to use the version of the practical reasoning scheme from Liverpool described in the user manual, and also add the credible source scheme.

- Fix or remove the Carneades examples on the web site.

- Remove all the pages of the old carneades.berlios.de web site

- Replace the links in the example pages of the new carneades web site to point to the examples in the Git repository, so that the content will  be under version control in the furture. 

- Change the name of the web app to "Carneades Web Application", instead of "IMPACT Policy Modelling Tool". 

- We need to redesign the "home page" of the web app a bit.  We need some way to choose/load a dataset (argument map, policies, etc) and then navigate between the components of the dataset.  (What should we call these datasets?)   For example, when I load  the Pierson v Post argument map, the policies button should no loner show the copyright policies about the Green Paper, since these have nothing to do with the Pierson v Post case.


- In argument maps, argument nodes should be colored after the argument graph has been evauated:

green: in  (also for con arguments)
red: out   (also for pro arguments)
white: undecided

- Further ideas for improving the visualization of the maps:  1) Let's use the same check boxes as in the outlines for display the in/out/undecided status of *both* argument and statement nodes.  (The check boxes need to be added to the argument nodes in the outline.  Currently they are shown only for statements.  2) Increase the size of the argument nodes to be the same size as the statement nodes, and add a label showing the name of the argumentation scheme applied.  3) Let's not color the arcs (lines) in the graph.  That is, let's not use red for links from con arguments to conclusions, or negated premises, nor green for links from pro arguments to conclusions and positive premises.  Let us black for all links.  Reason: to avoid overlaying two meanings for these colors, since we also use green/red/white to visualize the in/out/undecided status of argument and statement nodes.  I now think it may be confusing to use these same colors for different purposes in the same diagram. This is necessary for understanding how values are propogated throughout the graph.

- Possible bug:   accepting a conclusion of some argument which is out causes an argument which has this accepted statement as its only premise to be undecided (0.5) instead of in (1.0).  That is, accepting a statement does not override the arguments about the statement, but rather works as some kind of conflicting "argument" which causes conclusions depending on this statement to become undecided/uncertain.  Perhaps this is correct for grounded semantics, but it is much different than our previous system.  This needs to be checked.

- The user interfaces currently requires schemes to have headers with a title and maybe a description.  Should be optional.

- Need a validator for models/theories.

- Revise the Introduction in the user interface to make it independent of the chosen model.

- Bug in the dialog component (?)  when using cyclic rules.  See inverse rules for descendents and ancestors.

- Redesign the dialog component to ask all questions in a cateory, not matter which goal the inference engine tries to solve first.   (The order of the questions should respect the partial order defined by the :followups relation.)

- Simplify the installation process of the server?  One jar file, requiring just the .carneades.properites configuration file to be modified?
