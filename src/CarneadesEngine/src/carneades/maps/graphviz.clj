;;; Copyright ? 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

;;
;;
;; TODO port the code to the new engine / evaluation / colors
;; and have the same interface as i n carneades.maps.lacij
;;


(ns ^{:doc "Functions that convert an argument graph to the DOT (graphviz) 
            and PNG format by invoking the external 'dot' program.
            Graphviz needs to be installed http://www.graphviz.org/"}
  carneades.maps.graphviz
  (:use carneades.maps.viewerdef
      carneades.config.config
      carneades.engine.argument
      ;; [clojure.contrib.java-utils :only (delete-file)]
      [carneades.engine.statement :only (literal->str
                                         statement-complement)])
  ;; (:require [clojure.contrib.shell-out :as shell])
  )

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

(defn- get-id
  "Get the symbol used to identify some expr, generating
    one if the expr does not yet have an identifier.
    The identifers are suitable for naming nodes and arrows 
    in the DOT language, for use with GraphViz"
  [expr]
   (if-let [id (@*ids* expr)]
     id
     (let [newid (gensym "g")]
       (swap! *ids* assoc expr newid)
       newid)))

(defn- statement-graphvizstr [ag n stmt-str]
  (let [id (get-id n)]
    (format "    %s [shape=box, label=\"%s\", style=filled fillcolor=\"%s\"];\n"
            id
            (cond (and (in? ag n) (in? ag (statement-complement n)))
                  (str-escape "?? " (stmt-str n))
                  
                  (in? ag n) 
                  (str-escape "? " (stmt-str n))
                  
                  (in? ag (statement-complement n))
                  (str-escape "? " (stmt-str n))
                  
                  (questioned? ag n)
                  (str-escape "? " (stmt-str n))
                  
                  :else (stmt-str n))
            (cond (and (in? ag n) (in? ag (statement-complement n)))
                  "gold"
                  
                  (in? ag n)
                  "limegreen"
                  
                  (in? ag (statement-complement n))
                  "lightcoral"
                  
                  :else "white"))))

(defn- statements-graphvizstr [ag statements stmt-str]
  (str-join "" (map #(statement-graphvizstr ag % stmt-str) statements)))

(defn- argument-graphvizstr [ag arg]
  {:pre [(not (nil? arg))] }
  (str (format "    %s [shape=circle, label=\"%s\", color=\"%s\", style=filled, fillcolor=\"%s\"];\n"
               (get-id (argument-id arg))
               (if (= (:direction arg) :pro) "+" "-")
               (if (= (:direction arg) :pro) "forestgreen" "red")
               (cond (and (= :pro (:direction arg)) 
                          (applicable? ag arg))
                     "limegreen"
                     
                     (and (= :con (:direction arg)) 
                          (applicable? ag arg))
                     "lightcoral"
                     
                     :else "white"))
       (format "    %s -> %s [arrowhead=\"%s\", color=\"%s\"];\n"
               (get-id (argument-id arg))
               (get-id (argument-conclusion arg))
               (condp = (argument-direction arg)
                 :pro "normal"
                 :con "normal")
               (case (:direction arg)
                     :pro "forestgreen"
                     :con "red"))
       (str-join "" (map
                     #(format "    %s -> %s [style=\"%s\", color=\"%s\", arrowtail=\"%s\"];\n"
                              (get-id (premise-atom %))
                              (get-id (argument-id arg))
                              (if (premise-neg? %) "dashed" "solid")
                              (if (premise-neg? %) "red" "forestgreen")
                              "none")
                         (argument-premises arg)))))

(defn- arguments-graphvizstr [ag args]
  (str-join "" (map #(argument-graphvizstr ag %) args)))

(defn gen-graphvizcontent [ag stmt-str]
  (reset-ids)
  (str "digraph g {\n    rankdir = \"RL\";\n"
       (statements-graphvizstr ag
                               (map node-statement (get-nodes ag)) stmt-str)
       (arguments-graphvizstr ag (arguments ag))
       "}\n"))

(defn gen-image-from-dot [dotfile imgfile]
  (shell/sh *graphvizdot*
            (str "-T" *graphic-format*)
            dotfile
            "-o"
            imgfile))

(defn gen-image [ag stmt-str imgfile]
  (let [graphvizfile (gen-graphvizfile (gen-graphvizcontent ag stmt-str))]
    (gen-image-from-dot graphvizfile imgfile)
    (delete-file graphvizfile true)
    imgfile))

(defn- view-graphviz [ag stmt-str]
  (let [imgfile (str *tmpdir* *separator* (gensym "carneades") "."
                     *graphic-format*)]
    (gen-image ag stmt-str imgfile)
    (shell/sh *viewer* imgfile)))

(defmethod
  ^{:doc "argument-graph (statement -> string) -> nil

   Provides a convenient way to display an argument graph. 
   Based on code contributed by Andr?s F?rh?cz <fandrew@mit.bme.hu> 
   (in the original scheme version).
   To do: find a way to put the viewer process in the background,
   but still have the temporary files deleted after use."}
  ;; note: this can be done with temporary java files
  view-graph "graphviz"
  [viewer ag stmt-str]
  (view-graphviz ag stmt-str))
