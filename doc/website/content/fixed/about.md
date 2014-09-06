{
  "title": "Carneades",
  "sidebar": true,
  "weight": 1
}

The Carneades argumentation system provides web-based, collaborative
software tools for:

- (re)constructing arguments using a rulebase of argumentation schemes
- visualizing, browsing, navigating and editing argument graphs
- critically evaluating arguments, using a formal model of structured
  argument based on argumentation schemes and Dung abstract argumentation frameworks
- conducting structured opinion polls generated automatically from argument graphs
- evaluating the effects of policies, rules and laws, by using
  rulebases in interactive dialogues to construct arguments
- serializing and interchanging arguments using our own
  Carneades Argument Format (CAF) as well as the Argument Interchange Format
  (AIF)

Carneades is a multi-user Web application with a three-tiered
architecture consisting of a relational database backend layer, an
application logic layer and a web-based user interface. The system can be installed and used locally, without an internet connection.

An earlier, single-user desktop version of Carneades, called the
[Carneades Editor](https://github.com/carneades/carneades/releases/download/v1.0.2/carneades-editor-1.0.2.jar),
is still available.

Carneades is open source software licensed using the [European Union
Public License](http://ec.europa.eu/idabc/en/document/7774.html) (EUPL
v1.1). Note: Subsequent releases of Carneades will use another open source license, the [MPL-2.0](http://opensource.org/licenses/MPL-2.0) license. 

# Documentation

A [draft user
manual](https://github.com/carneades/carneades/blob/master/doc/manual/out/)
is available in various formats (PDF, HTML, ePub).
For programmers, online documentation of the [Carneades
APIs](http://carneades.github.com/doc/api) is also available.

# Getting Started

Prerequisites:

-   Version 7 or better of a [Java Runtime Environment (JRE)](https://www.java.com/en/).
-   A recent version of a standards-compliant Web Browser (e.g. Firefox,
    Chrome, Safari).

To download, install and run the Carneades Web Application

1.  Download the latest [release](https://github.com/carneades/carneades/releases/download/final-impact-review-rc0/carneades-impact-2013.zip)
    of the Carneades web application, named `carneades-impact-2013.zip`. *Warning: this is not the zip file shown at the top of this page.  That zip file contains the source files.*
2.  Unzip the `carneades-impact-2013.zip` file.
3.  Change to the `carneades-impact-2013` directory.
4.  Double click on the `carneades-webapp.jar` file or execute the
    following command line:\
     `$ java -jar carneades-webapp.jar`

    **Warning:** Microsoft Windows users may need to take extra steps to
    configure Java and the Carneades application. See [this
    explanation](http://answers.microsoft.com/en-us/windows/forum/windows_7-windows_programs/since-updating-to-windows-7-i-am-unable-to-run-any/b4b2c2fb-8634-4d26-bf76-a27cb7e6cbff)
    from Microsoft.

5.  The server will be start and open the home page of the Carneades
    client in your web browser.
6.  If the home page fails to open, enter this URL into the address bar
    of your web browser:\
     `http://localhost:8080/carneades/#/home`
7.  A list of projects should be shown. To get started, click on the
    link for the "Copyright in the Knowledge Economy" project and follow
    the instructions.
8.  To shutdown the server you may need to end the Java task using your
    task manager, depending on your operating system. If you started the
    server from the command line, you can kill the process with
    Control-C on Unix machines, including Macs. If you started the
    server on a Mac by double clicking on the jar file, you can shutdown
    the server by quitting the "routes\_selfexe" application in the
    usual way, with Command-Q.



