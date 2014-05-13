(ns carneades.engine.subag-test
  (:require [carneades.engine.argument-graph :refer [make-argument-graph
                                                     enter-statements
                                                     enter-arguments]]
            [carneades.engine.statement :refer [make-statement]]
            [carneades.engine.argument :refer [make-argument pm]]
            [carneades.engine.subag :refer [subag]]
            [midje.sweet :refer :all]
            [carneades.maps.lacij :refer [export]]
            [taoensso.timbre :as timbre :refer [debug info]]))

(fact "A subag at depth one focused on a statement contains only the
statement, its arguments and its premises"
      (let [ag (make-argument-graph)
            s1 (make-statement :id 's1)
            pa1-1 (make-statement :id 'pa1-1)
            pa1-2 (make-statement :id 'pa1-2)
            pa2-1 (make-statement :id 'pa2-1)
            pa2-2 (make-statement :id 'pa2-2)
            pa3-1 (make-statement :id 'pa3-1)
            pa4-1 (make-statement :id 'pa4-1)
            a1 (make-argument :id 'a1
                              :conclusion s1
                              :premises [(pm pa1-1)
                                         (pm pa1-2)])
            a2 (make-argument :id 'a2
                              :conclusion s1
                              :premises [(pm pa2-1)
                                         (pm pa2-2)])
            a3 (make-argument :id 'a3
                              :conclusion pa1-1
                              :premises [(pm pa3-1)])
            a4 (make-argument :id 'a4
                              :conclusion pa2-2
                              :premises [(pm pa4-1)])
            ag (enter-statements ag [s1 pa1-1 pa1-2 pa2-1 pa2-2 pa3-1 pa4-1])
            ag (enter-arguments ag [a1 a2 a3 a4])
            subag (subag ag 's1 1)
            s1' ((:statement-nodes subag) 's1)
            a1' ((:argument-nodes subag) 'a1)
            a2' ((:argument-nodes subag) 'a2)]
        (expect (set (:pro s1')) => '#{a1 a2})
        (expect (set (map :statement (:premises a1')))
                => '#{pa1-1 pa1-2})
        (expect (set (map :statement (:premises a2')))
                => '#{pa2-1 pa2-2})
        (expect (count (:statement-nodes subag)) => 5)
        (expect (count (:argument-nodes subag)) => 2)))

(fact "A subag at depth one focused on an argument contains only the
statement of the conclusion, the argument and its premises"
      (let [ag (make-argument-graph)
            s1 (make-statement :id 's1)
            pa1-1 (make-statement :id 'pa1-1)
            pa1-2 (make-statement :id 'pa1-2)
            pa2-1 (make-statement :id 'pa2-1)
            pa2-2 (make-statement :id 'pa2-2)
            pa3-1 (make-statement :id 'pa3-1)
            pa4-1 (make-statement :id 'pa4-1)
            a1 (make-argument :id 'a1
                              :conclusion s1
                              :premises [(pm pa1-1)
                                         (pm pa1-2)])
            a2 (make-argument :id 'a2
                              :conclusion s1
                              :premises [(pm pa2-1)
                                         (pm pa2-2)])
            a3 (make-argument :id 'a3
                              :conclusion pa1-1
                              :premises [(pm pa3-1)])
            a4 (make-argument :id 'a4
                              :conclusion pa2-2
                              :premises [(pm pa4-1)])
            ag (enter-statements ag [s1 pa1-1 pa1-2 pa2-1 pa2-2 pa3-1 pa4-1])
            ag (enter-arguments ag [a1 a2 a3 a4])
            subag (subag ag 'a1 1)
            s1' ((:statement-nodes subag) 's1)
            a1' ((:argument-nodes subag) 'a1)]
        (expect (set (:pro s1')) => '#{a1})
        (expect (set (map :statement (:premises a1')))
                => '#{pa1-1 pa1-2})
        (expect (count (:argument-nodes subag)) => 1)
        (expect (count (:statement-nodes subag)) => 3)))

