
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
1. Download the latest version of the file with a name of the form `carneades-webapp.x.x.x.zip`, from the [releases](https://www.dropbox.com/sh/dratrir8zh72ako/zSlrAC2K1D) page.
2. Unzip the `carneades-webapp.x.x.x.zip` [Zip](http://en.wikipedia.org/wiki/Zip_%28file_format%29) archive file using some Zip tool. This will create a directory (folder) with the following hierarchical structure

	- carneades
        * carneades-webapp.jar
        * config
          ** carneades.clj
		* doc
          ** manual.pdf
		  ** timestamp.txt
		* projects
        * README.txt
		
		
You can move this directory to some other location on your file system, at any time.

This creates a standard installation, with the default configuration.  


## Using the Web Application Locally 

To start the Carneades web application server double click on the `carneades-webapp.jar` file in your file system browser, for example the "Finder" on Mac OS X or the "Windows Explorer" on Windows PCs.

To start the server to from a command line, for local use, type

~~~
$ java -jar carneades-webapp.jar
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

3. Change directory to the CarneadesWebApp subdirectory.

 $ cd carneades/src/CarneadesWebApp

4. Build the Carneades system:

 $ lein sub install
 
5. Configure Carneades

- Copy the file config/carneades.clj into your $HOME directory and rename it to .carneades.clj (with a dot in front). Edit it and adapt the values to your configuration.

6. Starting the Carneades web application server

  $ lein ring server 8080

1. Using the Web client with your Web browser

- Point your browser to http://localhost:8080/carneades/#/introduction  

(*Note: Change "policymodellingtool" to "carneades" in these URLs.*)

## System Configuration

You can change the configuration of the system globally, for all users with accounts on your server. Alternatively, each user can have their own configuration.

To modify the global configuration, edit the `config/carneades.clj` file in the installation directory.  To create a personal configuration for your own user account, copy the `config/carneades.clj` file to a file named `.carneades.clj` in your home (user) directory and then edit this copy.

If a required property is not defined in the user's `.carneades.clj` file, then the value of the property defined in the `config/carneades.clj` file in the installation directory will be used.

The configure files are Clojure source files, with the properties represented as a Clojure map.

The following properties can be modified in the  `config/carneades.clj` file or overriden in `.carneades.clj` in your home directory. **Warning:** Be careful not to modify or delete any of the other properties in the `config/carneades.clj` file. The properties which should not be modified are clearly marked. 

`:schemes` 

: The full path name of the Clojure file containing the theory with the argumentation schemes to be used in projects by default. The default may be overriden on a project by project basis. (See the "Project File Structure" section of this manual for further information.) The argumentation schemes are defined using the scheme language described in [Modeling Policies and Argumentation Schemes](#modeling-policies-and-argumentation-schemes) chapter of this manual.

	Example: 

	`:schemes "/usr/local/carneades/schemes/walton_schemes.clj"

`:projects` 

:   The full path name of the directory used to store Carneades projects.  The default directory is the `projects` directory of the installation directory.

	Example:

	`:projects "/usr/local/carneades/projects" 

## Managing Projects

## Project File Structure

Carneades projects are each stored in a directory with the following structure:

~~~
properties.clj
databases/
theories/
documents/
~~~

The `properties.clj` file defines the attributes of the project. The properties are represented as a Clojure map in the file.  Here is the contents of an example properties.clj file:

~~~{.clojure}
{
 :policies "copyright_policies"
 :schemes  "default/walton_schemes"
}
~~~

The `:policies` and `:schemes` properties specify which theory to use to automatically construct arguments using the rule-based inference engine and which theory to use to interactively reconstruct arguments, respectively. 

Theories are represented using Clojure data structures, in Clojure files with the usual ".clj" filename extension.  In the property map, the values of the `:policies` and `:schemes` properties should be the file names of the Clojure files containing the theories, without the ".clj" file name extension.  These names are resolved relative to the "theories" directory of the project.  In the example, the `:policies` property is "copyright_policies". This refers to the theory in the "theories/copyright_policies.clj" file of the project.  A theory in another project can be referenced, by prefixing the name of the other project to the name of the theory. This is illustrated here by the `:schemes` property, which has the value "default/walton_schemes".  This references the "theories/walton_schemes.clj" file of the "default" project.  

The `databases/` directory stores all the database files of the project, including a database for each argument graph of the project. (A project may have than one argument graph. For example, when using the policy analysis tool, an argument graph is created for each case.) 

The `theories/` directory contains the Clojure source files of the theories of the project. Theories are rule-based models. They can be used for many purposes, including modeling argumentation schemes, policies, legislation and regulations.

Finally, the `documents/` directory can be used to store copies of source documents, documentary evidence, or any other project files. These files can be referenced and linked to in the descriptions of statements and nodes of argument graphs using relative URLs.  For example, the URL "file://src/foo.pdf" would be resolved to the file `documents/src/foo.pdf` of the project.

## Listing Projects

## Creating a New Project

## Renaming Projects and Editing Project Metadata

## Archiving Projects

## Deleting Projects



 
