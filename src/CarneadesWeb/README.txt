;;; Copyright © 2010 Fraunhofer Gesellschaft
;;; Licensed under the EUPL V.1.1

Carneades Web Project

Implementation of the Policy Modelling Tool for the European Framework 7 project IMPACT.

Interactive argument construction and evaluation using the Carneades argumentation library


Installation:

- download sources:
  - https://github.com/carneades/carneades/tree/webservice-01/src
  
- install leiningen:
  - https://github.com/technomancy/leiningen
  
- install maven (tested with maven 2.2.1):
  - http://maven.apache.org/download.html
  
- install jboss 5.1.0 GA:
  - http://sourceforge.net/projects/jboss/files/JBoss/JBoss-5.1.0.GA/
  - remove xercesImpl.jar from jboss installation : jboss-5.1.0.GA/lib/endorsed/xercesImpl.jar
  
- compile sources using maven 
  - compile Carneades in src/ :  mvn install
  - compile CarneadesWeb in src/CarneadesWeb/ : mvn install
  
- copy test files to /tmp/:
  - src/CarneadesWeb/test/IMPACT.xml and src/CarneadesWeb/test/translations.xml
  
- copy EAR to jboss deployment:
  - src/CarneadesWeb/CarneadesWeb-ear/target/CarneadesWeb-ear-ear
  - to: jboss-5.1.0.GA/server/default/deploy/
  
- run jboss:
  - jboss-5.1.0.GA/bin/run
  
- open browser:
  - http://localhost:8080/CarneadesWeb-web/index.html