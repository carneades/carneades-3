
# Browsing, Visualizing and Evaluating Arguments

This chapter of the Carneades user manual explains how to:

- Access and use the [home page](#the-argument-graph-home-page) of an argument graph on the World-Wide Web.

- Using hypertext to [browse an argument graph](#using-hypertext-to-browse-an-argument-graph).

- [Visualizing argument graphs](#visualizing-argument-graphs-in-argument-maps) in diagrams, called "argument maps", and using these maps to navigate to more detailed views of statements and arguments.

- [Evaluate arguments](#evaluating-arguments) to reveal missing premises, check the form of the argument, ask critical questions and assess the acceptability of statements.

- [Export an argument graph to XML](#exporting-argument-graphs-to-xml), to archive the graph or process it using other software.

- [Generate outlines](#generating-outlines) of the arguments in a graph, for further editing using text editors or word processors.

## The Argument Graph Home Page

The user interface of Carneades is a web application.  You access the pages and views of the user interface with web addresses, called Uniform Resource Locators (URL), just like you access any resource on the World-Wide Web. Most of the time you will access the application by clicking on a link embedded in some page on the Web, for example in a news article, blog entry or e-participation web site. If you are using Carneades as a stand-alone, desktop application, these URLs will be local addresses, from the "localhost" domain, pointing to web pages served by the application on your personal computer.   

![An Argument Graph Home Page](figs/homepage1.png)

The home page of an argument graph consists of the following parts:

- The *title* of the argument graph.  This title usually includes the topic of the discussion or debate.

- A *menu bar* of commands.  The commands shown depend on the role of the user.  Public users, who need not login to the system, are shown the commands "export" and "map", for exporting the argument graph to XML and viewing an argument map visualization of the graph, respectively. Analysts, who must login to the system with a password, are also shown "new statement" and "new argument" commands. Only analysts may modify the argument graph.

- A *description* of the topic of the discussion modeled in the argument graph. The description may be available in several languages. The user interface provides a way to select and change your preferred language during the session (*not yet implemented*). The description can be arbitrarily long and include multiple sections, paragraphs, images, hyperlinks, lists and other content. 

- A list of the *main issues* of the discussion.  Each item in the list is linked to a page providing detailed information about the statement in the argument graph at issue.

- An *outline* of the top five levels of the arguments in the argument graph.The first level of the outline lists the main issues (again). The second level lists the arguments pro and con each issue. The third level lists the premises of each of these arguments. The fourth level lists the argument pro and con each premise.  Finally, the fifth level lists the premises of these arguments. Deeper levels of the argument graph can be navigated to by first clicking on a statement or argument in the outline and then following the links on the next page. Since argument graphs may contain cycles and are not restricted to trees, some items may appear multiple times in the outline.

- A list of *references* to the source documents used to construct the argument graph.  For documents available on the Web, the reference will include a hyperlink to the source document. 

## Using Hypertext to Browse an Argument Graph 

There is a web page for each statement and argument in the argument graph providing detailed information about the element along with links to related statements and arguments in the graph. You can use these pages to navigate from node to node in the argument graph, by simply clicking on the links in the usual way. To go back to previous pages, use the back button of your web browser.  

### Statement Pages

![A Statement Page](figs/statement1.png)

The top of the statement page displays the properties of the statement: its id, atom, whether or not it is a main issue, its proof standard, usually "pe" (preponderance of the evidence), its weight and value. The other proof standards available are "dv" (dialectical validity), "cce" (clear and convincing evidence), and "brd" (beyond reasonable doubt). See the section on [Evaluating Arguments](#evaluating-arguments) for further details about proof standards.

The next section displays the *text* of the statement. This formulation of the statement is written by the analyst or analysts who reconstructed the arguments to build the argument graph. 

If metadata had been provided for the statement, it would be displayed next. Descriptions may be entered, by analysts, in multiple languages. The description, if available, will be displayed using the language chosen by the user. If no description has been entered manually by analysts for the selected language of the user but a description is available in some other language, a translation service will be used to generate a description in the selected language (*not yet implemented*).  

Finally, the statement pages lists pro and con arguments about the statement, i.e. arguments having this statement, or its negation, as a conclusion, as well as arguments which have this statement, or its negation, as a premise.  The premises of the pro and con arguments are also listed. This makes it possible to navigate to nearby arguments and statements in the argument graph, by simply clicking on the links in these lists.  Use then back button of your web browser to return to this statement page.

### Argument Pages

![An Argument Page](figs/argument1.png)

Argument pages are quite similar to statement pages. The top of an argument page displays the properties of the argument: its id, the argumentation scheme applied (if any), whether it is a strict or defeasible argument, its weight and value. 

If metadata had been provided for the argument, it would be displayed next.  The description, if available, will be displayed using the language chosen by the user. If no description has been entered manually by analysts for the selected language of the user but a description is available in some other language, a translation service will be used to generate a description in the selected language (*not yet implemented*).  

Next, the premises of the argument are listed.  If available, the role of each premise in the argumentation scheme applied is shown (e.g. "major" or "minor"). The check boxes to the left of each premise are used to indicate whether the statement is current *in* (☑, meaning presumably true), *out* (☒, meaning presumably false) or neither (☐, not enough information to presume either truth or falsity), given the arguments in the graph and the opinions of users from polls about the acceptability of statements and relative weights of pro and con arguments. 

After the premises, the conclusion of the argument is shown, preceded by "pro" or "con", showing the direction of the argument, and a check box showing the acceptability of the conclusion, as for the premises.

Finally, a list of counterarguments is shown.^[By counterarguments here we mean *rebuttals* (arguments with the opposite conclusion) and *undercutters* (arguments which deny the applicability of this argument).  Arguments which attack a premise of this argument ("undermining" arguments), are not listed. To navigate to undermining arguments, click on the premise of the argument of interest.  The undermining arguments will be listed on its statement page.]  The premises of the counterarguments arguments are also listed. This	 makes it possible to navigate to nearby arguments and statements in the argument graph, by simply clicking on the links in these lists.  Use then back button of your web browser to return to this statement page.

## Visualizing Argument Graphs in Argument Maps 

## Evaluating Arguments

## Exporting Argument Graphs to XML

## Generating Outlines

**Not yet implemented**
