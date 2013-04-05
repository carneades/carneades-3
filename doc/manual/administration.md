
# System Administration

## Downloading Carneades

The Carneades Argumentation System is open source software available on [Github](http://www.github.com) at <http://carneades.github.com/>. 

You can download Carneades as a ready-to-use web "binary" application from the [downloads](https://github.com/carneades/carneades/downloads) page. To use the version of Carneades described in this manual, download the latest file with a name of the form `carneades-webapp.x.x.x.zip`, where the x's are the version number.

The source code can be downloaded using Github from the [Carneades GitHub project page](https://github.com/carneades/carneades).  

## License

The source code of the Carneades system is licensed using the [European Public License](http://joinup.ec.europa.eu/software/page/eupl) (EUPL), Version 1.1. An English version of the license is distributed with the software, in the `/licenses` directory. The EUPL license is certified by the [Open Source Initiative](http://opensource.org/) (OSI). Like the [Gnu General Public License](http://www.gnu.org/licenses/gpl.html) (GPL), the EUPL is a recipriocal license. Derivative works of Carneades must also be licensed using either the EUPL *or a compatible license*. Several OSI-certified Open Source licenses are compatible with the EUPL. See the EUPL license for details.

## Prerequisites

- Version 7 or better of a [Java Runtime Environment (JRE 7)](http://www.oracle.com/technetwork/java/javase/downloads/index.html).

## Binary Installation

1. Prerequisites
 -  Version 7 or better of a [Java Runtime Environment (JRE 7)](http://www.oracle.com/technetwork/java/javase/downloads/index.html).
1. Download the latest version of the file with a name of the form `carneades-webapp.x.x.x.zip`, from the [downloads](https://github.com/carneades/carneades/downloads) page.
2. Unzip the `carneades-webapp.x.x.x.zip` [Zip](http://en.wikipedia.org/wiki/Zip_%28file_format%29) archive file using some Zip tool. This will create a directory (folder) with the following hierarchical structure

	- carneades
		* doc
		* bin
		* projects
		* config
		* schemes
		
You can move this directory to some other location on your file system, at any time.

This creates a standard installation, with the default configuration.  

To start the Carneades web application server double click on the `carneades-webapp.jar` file in your file system browser, for example the "Finder" on Mac OS X or the "Windows Explorer" on Windows PCs.

## Using the Web Application Locally 

To start the server to from a command line, for local use, type

~~~
$ java -jar bin/carneades-webapp.jar
~~~

Either way, after the server starts it will open up the "home page" of the Carneades web application in your default web browser.

Depending on your operating system and how you started the server, the Carneades web application can be shut down by either quitting the Carneades application or, if you started the server from a command line, using a terminal application, by ending this process , typically by typing `control-c` in the terminal.

(*Note: the Carneades process is currently named "routes_selfexe". This needs to be fixed.*)

## Using the Web Application with Java Application Servers

(*Note: to be written*)


## Source Code Installation

1. Prerequisites

- The [Java Runtime Environment (JRE) 7](http://www.oracle.com/technetwork/java/javase/downloads/index.html). (You may also use 
- Leiningen 2 <http://github.com/technomancy/leiningen>

2. Download the sources using the Git source code management system:

  $ git clone git://github.com/carneades/carneades.git

3. Change directory to the PolicyModellingTool subdirectory.

 $ cd carneades/src/PolicyModellingTool

4. Build the Carneades system:

 $ lein sub install
 
5. Configure Carneades

- Copy the file config/carneades.properties into your $HOME directory and rename it to .carneades.properties (with a dot in front). Edit it and adapt the values to your configuration.

6. Starting the Carneades web application server

  $ lein ring server 8080

1. Using the Web client with your Web browser

- Point your browser to http://localhost:8080/carneades/#/introduction  

(*Note: Change "policymodellingtool" to "carneades" in these URLs.*)

## System Configuration

You can change the configuration of the system globally, for all users with accounts on your server. Alternatively, each user can have there own configuration.

To modify the global configuration, edit the `config/carneades.properties` file in the installation directory.  To create a personal configuration for your own user account, copy the `config/carneades.properties` file to a file named `.carneades.properties` in your home (user) directory and then edit this copy.

If a required property is not defined in the user's `.carneades.properties` file, then the value of the property defined in the `config/carneades.properties` file in the installation directory will be used.

The configuration files files the format of a Java [".properties"](http://en.wikipedia.org/wiki/.properties) file.

The following properties can be modified in the  `config/carneades.properties` file or overriden in `.carneades.properties` in your home directory. **Warning:** Be careful not to modify or delete any of the other propertie in the `config/carneades.properties` file. The properties which should not be modified are clearly marked. 

`argumentation-schemes-file` 

:   The full path name of the file containing the argumentation schemes to be used in projects by default. The default may be overriden on a project by project basis. The argumentation schemes are defined using the scheme language described in [Modeling Policies and Argumentation Schemes](#modeling-policies-and-argumentation-schemes) chapter of this manual.

	Example: 

	`argumentation-schemes-file=/usr/local/carneades/schemes/walton-schemes.clj`

`projects-directory` 

:   The full path name of the directory used to store Carneades projects.  The default directory is the `projects` directory of the installation directory.

	Example:

	`projects-directory=/usr/local/carneades/projects` 

## Managing Projects

## Project File Structure

Carneades projects are stored in a directory with the following structure:

~~~
project.properties
cases/
db/
policies/
schemes/
documents/
~~~

The `project.properties` file defines the attributes of the project, such as its title. It has the format of a Java [".properties"](http://en.wikipedia.org/wiki/.properties) file.

The `cases/` directory stores the database files of the argument graph of the cases created by users when simulating the effects of the policies, when using the [opinion formation and polling tool](#formulating-polling-and-comparing-opinions). 

The `db/` directory contains the database files of the main argument graph of the project, containing the reconstructions of the argumetns in the source documents.

The `schemes/` directory contains models of argumentation schemes. The file with the model of the schemes currently used by the project is specified by the `argumentation-schemes-file` property in the `project.properties` file. The value of the property may be a relative pathname and will resolved relative to the `schemes` directory of the project. For example, suppose the `project.properties` file contains the line:

~~~
argumentation-schemes-file=walton.clj
~~~

This will be resolved to the `schemes/walton.clj` file in the project directory.

If no `argumentation-schemes-file` property is defined for the project the value of the property in the user's `.carneades.properties` file, in his or her home directory will be used, if it is defined there. Otherwise the value of the property in the `config/carneades.properties` in the installation directory will be used.

This general pattern applies to all properties. The value in the `project.properties` file overrides the value in the user's Carneades configuration file, which in turn overrides the default values in the `config` directory of the installation directory.

Providing a directory for schemes allows multiple sets of argumentation schemes to be stored together with the project. But only set of schemes is active at any time.

Finally, the `documents/` directory can be used to store copies of source documents used when reconstructing arguments, or any other projects files. These files can be referenced and linked to in the descriptions of statements and nodes using relative URLs.  For example, the URL "src/foo.pdf" would be resolved to the file `documents/src/foo.pdf` in the `documents` directory of the project.

## Listing Projects

## Creating a New Project

## Renaming Projects and Editing Project Metadata

## Archiving Projects

## Deleting Projects



 
