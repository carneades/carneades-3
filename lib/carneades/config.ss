(module config mzscheme
  
  (provide (all-defined))
  
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
       
  )