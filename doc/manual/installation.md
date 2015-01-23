# Getting Started

This chapter explains how to download, install and run the Carneades
system locally on your personal computer. For information about how to
build the system from source code and advanced configuration options
see the "System Administration" chapter.

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

	- carneades-webapp
        * carneades-webapp-0.7.0.jar
        * config 
            ** carneades.clj
		* doc 
            ** manual.pdf 
            ** timestamp.txt
		* projects
        * README.txt
		
You can move this directory to some other location on your file
system, at any time.

This creates a standard installation, with the default configuration.

### Using the Web Application Locally

To start the Carneades web application server double click on the
`carneades-webapp-0.7.0.jar` file in your file system browser, for example
the "Finder" on Mac OS X or the "Windows Explorer" on Windows PCs.

To start the server to from a command line, for local use, type

~~~
$ java -jar carneades-webapp.0.7.0.jar
~~~

Either way, after the server starts it will open up the projects page
of the Carneades web application in your default web browser.

Depending on your operating system and how you started the server, the
Carneades web application can be shut down by either quitting the
Carneades application or, if you started the server from a command
line, using a terminal application, by ending this process, typically
by typing `control-c` in the terminal.


