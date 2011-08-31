;;; Copyright © 2010 Fraunhofer Gesellschaft
;;; Licensed under the EUPL V.1.1

IMPACT Policy Modelling Tool

Implementation of the Policy Modelling Tool for the European Framework 7 project IMPACT.

Interactive argument construction and evaluation using the Carneades argumentation library


Installation (using Tomcat):

- download & install git
  - http://git-scm.com/

- download sources and switch to webservice branch:
  - git clone git://github.com/carneades/carneades.git
  - git checkout -b webservice-01 remotes/origin/webservice-01
  
- download & install leiningen:
  - https://github.com/technomancy/leiningen
  
- download & install maven (tested with maven 2.2.1):
  - http://maven.apache.org/download.html
  
- download & install Apache Tomcat 7.0
  - http://tomcat.apache.org/download-70.cgi
  
- compile sources using maven 
  - compile Carneades in src/ :  mvn install
  - compile PMT in src/PolicyModellingTool/ : mvn install
  
- copy test files to /tmp/:
  - src/PolicyModellingTool/test/IMPACT.xml 
  - src/PolicyModellingTool/test/translations.xml
  
- copy WAR to Tomcat deployment:
  - src/PolicyModellingTool/target/PolicyModellingTool.war
  - to: apache-tomcat-7.0.x/webapps/
  
- run Tomcat:
  - apache-tomcat-7.0.x/bin/catalina run
  
- open browser:
  - http://localhost:8080/PolicyModellingTool
 