
# System Administration

## Building and Installing from Source Code 

1. Use a [Git](http://git-scm.com/) client to clone a copy of the
Carneades source code from the
[Carneades GitHub repository](https://github.com/carneades/carneades).

2. Follow the instructions in the following README file:

~~~
carneades/src/CarneadesWeb/README.md
~~~

## System Configuration

To modify the configuration, edit the `config/carneades.clj`
file in the installation directory.

The configure files are Clojure source files, with the properties
represented as a Clojure map.

The `:projects` property provides the full path name of the directory
used to store Carneades projects. The default directory is the
`projects` directory of the installation directory.

Example: `:projects "/usr/local/carneades/projects`

Additional properties can be configured for each project, as described
in the next section.

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
files.

<!--
These files can be referenced and linked to in the descriptions
of statements and nodes of argument graphs using relative URLs.  For
example, the URL "file://src/foo.pdf" would be resolved to the file
`documents/src/foo.pdf` of the project.
-->
 
