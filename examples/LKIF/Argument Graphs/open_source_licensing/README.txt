	
# Instructions for using the Open Source License Compatibility Application
The directory contains the source code of a pilot application of the Carneades argumentation system which aims to provide software developers with a tool for analyzing open source license compatibility issues.  This application was developed in the European [Qualipso](http://www.qualipso.org/) project.  

# Files

Here's a brief description of the files in this directory:

- README.txt.   This file
- oss-licenses.owl.   An ontology, in the Web Ontology Language (OWL), of open source license concepts and for describing dependencies and use relationships between the components of software systems and their licenses.
- oss-rules.xml.  A rulebase, in the [Legal Knowledge Interchange Format](http://carneades.github.com/resources/Home/LKIF-Specification.pdf) (LKIF), of example interpretations of some copyright concepts, in particular the concept of a derivative work, needed  for analysing open source license compatibility issues.
- impact-licensing.owl.  An description, in OWL, of a hypothetical software system, based roughly on an argumentation toolbox currently being designed in the European IMPACT project for policy deliberations on the Internet.
- impact-kb.xml.  An LKIF file which imports the ontology and rules and defines an argument map containing some assumptions and an issue to investigate. 

# Prerequisites

- A recent version of the [Java Runtime Environment](http://www.java.com/), from Oracle.

# Running the Application 

1. From the [Carneades download directory](https://github.com/carneades/carneades/downloads) on Github, download the latest versions of:

    - [The Carneades Editor](https://github.com/downloads/carneades/carneades/carneades-editor-1.0.1.jar),
    - [Walton.zip](https://github.com/downloads/carneades/carneades/Walton.zip)

2. The editor is a portable Java desktop application, with a graphical user interface.  The application can be started from a command line:

    $ java -jar carneades-editor-1.0.1.jar

    Using graphical user interfaces, such as the Windows Explorer and the Macintosh Finder, the Carneades Editor can be started by double clicking on the file in the user interface.

3. Create a directory (folder) for argumentation schemes and unzip the Walton.zip file you downloaded in step 1 into this directory.

4. Create a directory for storing knowledge bases consisting of ontologies in the Web Ontology Language (OWL) and rules in the Legal Knowledge Interchange Format (LKIF).  Carneades will look for OWL and LKIF files in this directory when relative path names are used. This directory may be organized using subdirectories.

5. Set the Argumentation Schemes File and Rules Directory properties using the Edit/Preferences command.  Set the Argumentation Schemes File property to the full path of the "walton-schemes.xml" file in the Walton folder created when you unzipped the "Walton.zip" file. Set the Rules Directory to the full path of the directory you created above.

6. Open the "impact-kb.xml" file in the Carneades Editor, using the File/Open command.

7. Select the box in the right panel containing the text "mayUseLicenseTemplate CarneadesEngine GPL_Template".  This is the main issue we want to analyze.

8. Use the "Assistant/Find Arguments" command to search for arguments pro and con this issue. Set the maximum number of nodes to visit in the search space to 100 and the maximum number of turns taken by pro and con to 3. Then click on the "next" button.  Wait for the search process to complete. Then click on the "finish" button.  The arguments found should now be displayed in the argument map.

9. Experiment with the affects of accepting or rejecting different propositions (statements) in the graph, by clicking on a statement node and the changing its status (stated, questioned, accepted, rejected) in the properties panel on the left. You might also want to change the proof standard assigned to some statement.  The default proof standard is "preponderance of the evidence".

10. Finally, to find minimal positions which would make the main issue in or out, using abduction, use the "assistant/find positions" command.  Minimize the positions found, removing positions which are supersets of other positions, by checking the "minimize" box. Scroll through the positions found with the large green arrows. Select a statement in the list of some position which interests you. Click on the "finish" button to have the selected statement shown and highlighted in the argument map.
 

# Further Information

For further information about this pilot application, see Qualipso Deliverable A1.D2.1.3, available for downloading at the  these locations:

- <http://www.qualipso.org/documents>
- <http://www.tfgordon.de/publications/files/QualipsoA1D213.pdf>
 
The report can be cited as follows:

Gordon, T.F. Report on a Prototype Decision Support System for OSS License Compatibility Issues. Qualipso (IST- FP6-IP-034763), Deliverable A1.D2.1.3, 2010.




