;;; Carneades Argumentation Library and Tools.
;;; Copyright (C) 2010 Thomas F. Gordon, Fraunhofer FOKUS, Berlin
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

(ns carneades.ui.diagram.graphvizviewer
  (:use clojure.contrib.def
        clojure.contrib.str-utils
        clojure.contrib.duck-streams
        carneades.ui.diagram.viewerdef
        carneades.config.reader
        carneades.engine.argument
        [clojure.contrib.java-utils :only (delete-file)]
        [carneades.engine.statement :only (statement-formatted
                                           statement-complement)])
  (:require [clojure.contrib.shell-out :as shell]))

(set! *assert* true)

(defvar- *viewer* (configvalue "viewer"))
(defvar- *graphic-format* (configvalue "graphviz.format"))
(defvar- *graphvizdot* (configvalue "graphviz.dot"))
(defvar- *tmpdir* (configvalue "graphviz.tmpdir"))

(defvar- *separator* java.io.File/separator)

(defn- str-escape
  "like str but replace newlines (\n) by \\n"
  ([x] (re-gsub #"\n" "\\\\n" (str x)))
  ([x & strs]
     (str-join "" (map str-escape (cons x strs)))))

(defn- gen-graphvizfile [content]
  (let [filename (str *tmpdir* *separator* (gensym "carneades") ".dot")]
    (spit filename content)
    filename))

(defvar- *ids* (atom {}))

(defn- reset-ids []
  (reset! *ids* {}))

(defn- get-id [expr]
   "Get the symbol used to identify some expr, generating
    one if the expr does not yet have an identifier.
    The identifers are suitable for naming nodes and arrows 
    in the DOT language, for use with GraphViz"
   (if-let [id (@*ids* expr)]
     id
     (let [newid (gensym "g")]
       (swap! *ids* assoc expr newid)
       newid)))

(defn- statement-graphvizstr [ag n stmt-str]
  (let [id (get-id n)]
    (format "    %s [shape=box, label=\"%s\", style=\"%s\"];\n"
            id
            (cond (questioned? ag n) (str-escape "? " (stmt-str n))
                  (accepted? ag n) (str-escape  "+ " (stmt-str n))
                  (rejected? ag n) (str-escape  "- " (stmt-str n))
                  :else (str-escape (stmt-str n)))
            (cond (and (acceptable? ag n)
                       (acceptable? ag (statement-complement n)))
                  "dotted,filled"
                  (acceptable? ag n) "filled"
                  (acceptable? ag (statement-complement n)) "dotted"
                  :else "solid"))))

(defn- statements-graphvizstr [ag statements stmt-str]
  (str-join "" (map #(statement-graphvizstr ag % stmt-str) statements)))

(defn- argument-graphvizstr [ag arg]
  {:pre [(not (nil? arg))] }
  (str (format "    %s [shape=ellipse, label=\"%s\", style=\"%s\"];\n"
               (get-id (argument-id arg))
               (if (argument-scheme arg)
                 (argument-scheme arg)
                 (str-escape (argument-id arg)))
               (if (applicable? ag arg)
                 "filled"
                 "solid"))
       (format "    %s -> %s [arrowhead=\"%s\"];\n"
               (get-id (argument-id arg))
               (get-id (argument-conclusion arg))
               (condp = (argument-direction arg)
                 :pro "normal"
                 :con "onormal"))
       (str-join "" (map
                     #(format "    %s -> %s [style=\"%s\", arrowhead=\"%s\"];\n"
                              (get-id (premise-atom %))
                              (get-id (argument-id arg))
                              (cond (assumption? %) "dotted"
                                    (exception? %) "dashed"
                                    :else "solid")
                              (if (premise-neg? %) "tee" "none"))
                         (argument-premises arg)))))

(defn- arguments-graphvizstr [ag args]
  (str-join "" (map #(argument-graphvizstr ag %) args)))

 (defn- gen-graphvizcontent [ag stmt-str]
   (reset-ids)
   (str "digraph g {\n    rankdir = \"RL\";\n"
        (statements-graphvizstr ag
                                (map node-statement (get-nodes ag)) stmt-str)
        (arguments-graphvizstr ag (arguments ag))
        "}\n"))

(defn- view-graphviz [ag stmt-str]
  (let [graphvizfile (gen-graphvizfile (gen-graphvizcontent ag stmt-str))
        imgfile (str *tmpdir* *separator* (gensym "carneades") "."
                     *graphic-format*)]
    (shell/sh *graphvizdot*
                     (str "-T" *graphic-format*)
                     graphvizfile
                     "-o"
                     imgfile)
    (delete-file graphvizfile true)
    (shell/sh *viewer* imgfile)))

(defmethod view-graph "graphviz" [viewer ag stmt-str]
  "argument-graph (statement -> string) -> nil

   Provides a convenient way to display an argument graph. 
   Based on code contributed by András Förhécz <fandrew@mit.bme.hu>.
   To do: find a way to put the viewer process in the background,
   but still have the temporary files deleted after use."
  (view-graphviz ag stmt-str))

