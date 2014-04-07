;; Copyright (c) 2012 Fraunhofer Gesellschaft
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns carneades.maps.format-statement
  (:use carneades.engine.argument
        carneades.engine.statement
        carneades.engine.argument-evaluation)
  (:require [clojure.string :as s]))


(def max-len 30)

(defn shorten [word]
  (if (> (count word) max-len)
    (str (subs word 0 (- max-len 2)) "..")
    word))

(defn make-line [words]
  (letfn [(append
           [line word]
           (if (empty? line)
             word
             (str line " " word)))]
   (loop [taken 0
          len 0
          words words
          line ""]
     (let [word (first words)
           l (count word)]
       (cond (empty? words)
             {:words words :line line :taken taken}

             (> l max-len)
             (if (zero? taken)
               (let [word (shorten word)
                     line (append line word)]
                 {:words (rest words) :line line :taken (inc taken)
                  :last-truncated true})
               {:words words :line line :taken taken})

             (> (+ len l) max-len)
             {:words words :line line :taken taken}
            
             :else
             (recur (inc taken)
                    (+ len l)
                    (rest words)
                    (append line word)))))))


(defn trunk [s]
  (if (nil? s)
    ""
    (let [words (s/split s #"\s+")
          {words :words line1 :line} (make-line words)
          {words :words line2 :line} (make-line words)
          {words :words line3 :line last-truncated :last-truncated} (make-line words)]
      (cond (and (nil? line2) (nil? line3))
            line1

            (nil? line3)
            (str line1 "\n" line2)
            
            :else
            (str line1 "\n" line2 "\n" line3
                 (cond (and last-truncated (not (empty? words)))
                       "."

                       (not (empty? words))
                       "..."
                       
                       :else nil))))))

(defn trunk-line
  [s]
  (let [s (trunk s)
        lines (s/split-lines s)]
    (if (= (count lines) 1)
      (first lines)
      lines)))

(defn stmt-to-str [stmt stmt-str]
  (let [formatted (stmt-str (map->statement stmt))]
    (cond (in-node? stmt) (str "✔ " formatted) 
          (out-node? stmt) (str "✘ " formatted)
          (undecided-node? stmt) formatted
          :else (throw (Exception. "Invalid case")))))


