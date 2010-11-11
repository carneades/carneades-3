
The directory contains the source code of a pilot application of the Carneades argumentation system which aims to provide software developers with a tool for analyzing open source license compatibility issues.  This application was developed in the European Qualipso project.  

Here's a brief description of the files in this directory:

- README.txt.   This file
- oss-licenses.owl.   An ontology, in the Web Ontology Language (OWL), of open source license concepts and  for describing dependencies and use relationships between the components of software systems and their licenses.
- oss-rules.xml.  A rulebase, in the Legal Knowledge Interchange Format, of example interpretations of some copyright concepts, in particular the concept of a derivative work, needed  for analysing open source license compatibility issues.
- impact-licensing.owl.  An description, in OWL, of a hypothetical software system, based roughly on an argumentation toolbox currently being designed in the European IMPACT project for policy deliberations on the Internet.
- impact_licensing.clj.  A script in the Clojure programming language for applying the ontology and rules to the description of the example software system which produces, as output, an LKIF file containing arguments pro and con the compatibility of the GNU GPL with the licenses of the components used by the system.   To run the script, you will need to download and install the Carneades Engine from the Berlios GIT repository at:

     http://developer.berlios.de/git/?group_id=9385

The resulting arguments, in LKIF format, can be viewed using the Carneades Editor, an argument mapping application for the Java Virtual Machine.  The Carneades Editor can be downloaded from the Carneades web site here:

           http://carneades.berlios.de/

For further information about this pilot application, see Qualipso Deliverable A1.D2.1.3, available for downloading in the Legal Issues section of the following page of the Qualipso web site:

      http://www.qualipso.org/documents

The report can be cited as follows:

Gordon, T.F. Report on a Prototype Decision Support System for OSS License Compatibility Issues. Fraunhofer FOKUS, Berlin, 2010.



