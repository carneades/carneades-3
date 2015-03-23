
# System Administration

## Downloading Carneades

The Carneades Argumentation System is open source software available
at <https://github.com/carneades>.

You can download binaries and source code of Carneades releases at
from the [releases](https://github.com/carneades/carneades/releases)
page. To use the version of Carneades described in this manual,
download the release named "Carneades WebApp 0.7".

## License

The source code of the Carneades system is licensed using the
[Mozilla Public License (MPL) version 2.0](http://www.mozilla.org/MPL/). An
English version of the license is distributed with the software, in
the `/licenses` directory. The MPL license is certified by the
[Open Source Initiative](http://opensource.org/) (OSI).

> The MPL is a simple copyleft license. The MPL's "file-level"
> copyleft is designed to encourage contributors to share
> modifications they make to your code, while still allowing them to
> combine your code with code under other licenses (open or
> proprietary) with minimal restrictions.

In particular, if you write an application which links to Carneades as
a library, you are free to use any license you wish for your own code.

See [http://www.mozilla.org/MPL/2.0/FAQ.html](FAQ) for more
information.

## Binary Installation

Prerequisites:

- Version 7 or better of a [Java Runtime Environment](https://www.java.com/).

Installation Procedure:

1. Download the `carneades-webapp.0.7.0.zip` file from Carneades WebApp 0.7 release on the [releases](https://github.com/carneades/carneades/releases) page. 
2. Unzip the `carneades-webapp.0.7.0.zip`
   [Zip](http://en.wikipedia.org/wiki/Zip_%28file_format%29) archive
   file using some Zip tool. This will create a directory (folder)
   with the following hierarchical structure

*TODO: update the file list below*

	- carneades
        * carneades-webapp-0.7.0.jar
        * config ** carneades.clj
		* doc ** manual.pdf ** timestamp.txt
		* projects
        * README.txt
		
You can move this directory to some other location on your file
system, at any time.

This creates a standard installation, with the default configuration.

## Using the Web Application Locally

To start the Carneades web application server double click on the
`carneades-webapp-0.7.0.jar` file in your file system browser, for example
the "Finder" on Mac OS X or the "Windows Explorer" on Windows PCs.

To start the server to from a command line, for local use, type

~~~
$ java -jar carneades-webapp.0.7.0.jar
~~~

Either way, after the server starts it will open up the projects page of
the Carneades web application in your default web browser.

Depending on your operating system and how you started the server, the
Carneades web application can be shut down by either quitting the
Carneades application or, if you started the server from a command
line, using a terminal application, by ending this process , typically
by typing `control-c` in the terminal.

## Source Code Installation

*TODO: Check and update this section*

1. Prerequisites

- Version 7 or better of a [Java Runtime Environment](https://www.java.com/).
- A Unix operating system. (We tested the installation process using Ubuntu 14.04 LTS.)
- Leiningen  <http://github.com/technomancy/leiningen>
- Ruby >=1.9 (2.x recommended)
- Node package manager (npm) provided by [node.js](http://nodejs.org/)

2. Download the sources using the Git source code management system:

  $ git clone https://github.com/carneades/carneades.git

3. Change directory to the scripts subdirectory:

 $ cd carneades/src/CarneadesWebApp/scripts

4. Execute the installation script, also in the scripts directory:

 $ ./install.sh
 
5. Configure Carneades

- Copy the file config/carneades.clj into your $HOME directory and
  rename it to .carneades.clj (with a dot in front). Edit it and adapt
  the values to your configuration. (See the "System Configuration"
  section below for details.)

6. Starting the Carneades web application server

In the scripts directory, execute:

  $ ./start.sh

7. You can now access the web user interface using the following URL:

- http://localhost:8081/carneades/index.html


## System Configuration

You can change the configuration of the system globally, for all users
with accounts on your server. Alternatively, each user can have their
own configuration.

To modify the global configuration, edit the `config/carneades.clj`
file in the installation directory.  To create a personal
configuration for your own user account, copy the
`config/carneades.clj` file to a file named `.carneades.clj` in your
home (user) directory and then edit this copy.

If a required property is not defined in the user's `.carneades.clj`
file, then the value of the property defined in the
`config/carneades.clj` file in the installation directory will be
used.

The configure files are Clojure source files, with the properties
represented as a Clojure map.

The following properties can be modified in the `config/carneades.clj`
file or overriden in `.carneades.clj` in your home
directory. **Warning:** Be careful not to modify or delete any of the
other properties in the `config/carneades.clj` file. The properties
which should not be modified are clearly marked.

`:projects`

: The full path name of the directory used to store Carneades
projects.  The default directory is the `projects` directory of the
installation directory.

	Example:

	`:projects "/usr/local/carneades/projects"

Additional properties can be configured for each project, as described next.

## Project Structure and Configuration

Carneades projects are stored in the directory stated in the
`.carneades.clj` configuration file. Each project is a directory with
the following structure:

~~~
properties.clj
databases/
theories/
documents/
~~~

The `properties.clj` file defines the attributes of the project. The
properties are represented as a Clojure map in the file.  Here is the
contents of an example properties.clj file:

~~~{.clojure}
{
:title "My First Carneades Project"
:creation-date "January 10, 2015"
:theory "default/walton_schemes"
:description {:en "This is my first Carneades project."
              :de "Dies ist mein erstes Carneades Projekt."}
}
~~~

The `:title` and `:creation-date` properties should be
self-explanatory. Their values are strings. (The creation date is not
constrained to some specific format for dates.)

The `:description` is a Clojure map, to allow multiple descriptions in
different languages. In the example there are descriptions in English
and German.  Descriptions can formatted using
[Markdown](https://en.wikipedia.org/wiki/Markdown) syntax.

The `:theory` property specifies which theory to use
to construct and reconstruct arguments, using the rule-based inference
engine, or interactively, using the argument editor.

Theories are represented using Clojure data structures, in Clojure
files with the usual ".clj" filename extension.  In the property map,
the values of the `:theory` property should be the file name of the
Clojure file containing the theory, but without the ".clj" file name
extension.  The theory name is resolved relative to the "theories"
directory of the given project, or the current project being
configured if no project is named.  In the example, the `:theory`
property is "default/walton_schemes". This refers to the theory in the
"default/theories/walton_schemes.clj" file in the projects directory.

The `databases/` directory stores all the database files of the
project, including a database for each argument graph of they
project. (A project may have than one argument graph. For example,
when using the policy analysis tool, an argument graph is created for
each case.) The database files are in the format used by the
[H2](http://www.h2database.com/) database engine for the Java Virtual
Machine.

The `theories/` directory contains the Clojure source files of the
theories of the project. 

Finally, the `documents/` directory can be used to store copies of
source documents, documentary evidence, or any other project
files. These files can be referenced and linked to in the descriptions
of statements and nodes of argument graphs using relative URLs.  For
example, the URL "file://src/foo.pdf" would be resolved to the file
`documents/src/foo.pdf` of the project.


 
