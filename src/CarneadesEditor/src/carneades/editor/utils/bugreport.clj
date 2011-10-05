;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns ^{:doc "Launches the defaut email program to report a bug."}
  carneades.editor.utils.bugreport
  (:use clojure.contrib.def)
  (:import java.awt.Desktop
           java.net.URI
           (java.io StringWriter PrintWriter)
           sun.misc.BASE64Decoder))

;; base64 encoded email adress, to protect from spams

(defvar- *recipient*
  (String. (.decodeBuffer (BASE64Decoder.)
                          "Y2FybmVhZGVzLmJ1Z3NAZ21haWwuY29t")))

(defvar- *mail-content*
  (str
   "?SUBJECT=Carneades Bug Report - <summary>"
   "&BODY=[Summary]\n\n"
   "[Description]\n\n"
   "[Steps to reproduce]\n\n"
   "[Additional information]\n\n"
   "Exception message: %s\n\n"
   "Stacktrace:\n\n"
   "%s\n"))

(defvar *desktop* (Desktop/getDesktop))

(defn report-bug [exception]
  (let [sw (StringWriter.)
        pw (PrintWriter. sw)
        stackstr (.printStackTrace exception pw)
        uri (URI. "mailto"
                  (str
                   *recipient*
                   (format *mail-content* (.getMessage exception)
                           (str sw)))
                  nil)]
    (.mail *desktop* uri)))
