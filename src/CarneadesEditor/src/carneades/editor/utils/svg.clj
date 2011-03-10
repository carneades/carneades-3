;; The other day, I wanted to make some graph paper.

;; While experimenting with inkscape, I noticed that svg is actually an xml file format.

;; Since clojure is good with xml, that means that it's actually easier to make such a drawing with a program:

;; Each element of the drawing is represented as a map

(defn make-rect [i j squaresize]
  {:tag :rect
   :attrs {:x (str (* squaresize i))
           :y (str (* squaresize j))
           :width  (str squaresize)
           :height (str squaresize)
           :style "fill:white;stroke:black;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1"}})

;; The whole file is represented as a map containing those maps

(defn make-svg [gridsize squaresize]
  {:tag :svg
   :attrs {:width "100%"
           :height "100%"
           :version "1.1"
           :xmlns "http://www.w3.org/2000/svg"}
   :content (for [i (range gridsize) j (range gridsize)] (make-rect i j squaresize))})

;; The library clojure.contrib.lazy-xml will turn the nested map into xml:

(require 'clojure.contrib.lazy-xml)

;; We can use with-out-str to capture the output, which is unaccountably printed
;; rather than given back as a string, and spit to write it to a file.

(spit "/tmp/squares.svg" (with-out-str (clojure.contrib.lazy-xml/emit (make-svg 10 80)
                                                                      :indent 4)))

;; The nice thing about this is that you can then use inkscape to modify the
;; file, and then diff to work out how to take the modifications back into the
;; program. Does anybody know how to make the emit function format the xml so
;; that the output file is nicely readable?



