;;; Carneades Argumentation Library and Tools.
;;; Copyright (C) 2008 Thomas F. Gordon, Fraunhofer FOKUS, Berlin
;;; 
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU Lesser General Public License version 3 (LGPL-3)
;;; as published by the Free Software Foundation.
;;; 
;;; This program is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for details.
;;; 
;;; You should have received a copy of the GNU Lesser General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
 

#!r6rs

(library 
 (carneades config)
 
 (export dot preferred-graphic-format viewer tmpdir)
 (import (rnrs base))
 
 ; global configuration parameters
 
 ; dot: path to the dot program for converting dot digrams. See http://www.graphviz.org
 
 ; Example for Unix users, including Mac OS X and Linux:
 ; (define dot "/usr/local/graphviz-2.14/bin/dot")
 (define dot "/Applications/Graphviz/Graphviz.app/Contents/MacOS/dot")
 
 ; Example For Windows users.  This works if dot.exe is in a folder in your PATH.
 ; (define dot "dot.exe")
 
 ; preferred-graphic-format:  "dot" "png" | "ps" | "svg"
 (define preferred-graphic-format "dot")
 
 ; viewer: path to a program for viewing files in the preferred graphic format.
 ; The program must accept a filename as its first parameter
 
 ; For SVG and PNG files the open source FireFox web browser can be used.  See http://en.www.mozilla.com/en/firefox/
 ; For SVG another possibility is the open source Batik Squiggle program.  It is written in Java and should run
 ; on most operating systems.  See http://xmlgraphics.apache.org/batik/
 
 ; Mac OS X examples:
 ; (define viewer "/Applications/Firefox.app/Contents/MacOS/firefox-bin")
 ; (define viewer "java -jar /Applications/Batik/batik-squiggle.jar")
 (define viewer "/Applications/Graphviz/Graphviz.app/Contents/MacOS/Graphviz")
 
 ; Windows examples.  Notice the double backslashes, which are needed since backslash
 ; is the escape character in Scheme, and the backslashed quotation marks, which are needed
 ; to enclose the entire path name in quotation marks due to the spaces in "Program Files" and
 ; "Mozilla Firefox".
 ; (define viewer "\"C:\\Program Files\\Mozilla Firefox\\firefox.exe\"")
 ; (define viewer "java -jar C:\\Batik\\batik-squiggle.jar")
 
 ; tmpdir: filename of a directory for temporary files.  Be sure to include the 
 ; final pathname separation character, '/' (Unix) or '\\' (Windows)
 (define tmpdir "/tmp/")
 )