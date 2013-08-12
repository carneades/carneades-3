;;; taken from clojurescriptone.org
(ns ^{:doc "Event dispatching.

  Provides a way for code to react to events. Terminology:

  * event-id: Identifies a class of events. Can be any Clojure value.

  * event-data: Parameterizes a particular event. Can be any Clojure
    value.

  * reactor: A function that is invoked in response to an event
    occurring.

  * reaction: A relationship between a set of events and a reactor.

  * event-pred: A function which takes an event ID and returns true or
    false.

  Reactors are associated with events via `react-to`. When events are
  fired with an `event-id` and optional `event-data`, any reactors whose
  `event-pred` returns true for the `event-id` are invoked."}
  carneades.analysis.web.dispatch)

(def ^{:doc "Stores the current reactions."}
  reactions (atom {}))

(defn react-to
  "Cause the specified reactor to be invoked whenever an event that
  satisfies `event-pred` is fired. reactor is a function that accepts
  two arguments: `event-id` and `event-data`.

  Returns the reaction.

  The reactor will continue to be invoked until one of two things
  happens:

   1. `delete-reaction` is called on this reaction.

   2. The reaction occurs `max-count` times. If `max-count` is not
      specified, the reaction will continue to be invoked until deleted.

  If `max-count` is specified, `delete-reaction` will be called
  automatically when the reaction has occurred the specified number of
  times."
  ([event-pred reactor]
     (react-to nil event-pred reactor))
  ([max-count event-pred reactor]
     (let [reaction {:max-count max-count
                     :event-pred event-pred
                     :reactor reactor}]
       (swap! reactions assoc reaction 0)
       reaction)))

(defn delete-reaction
  "Delete a reaction. After calling this function, the specified
  reaction will no longer be invoked."
  [reaction]
  (swap! reactions dissoc reaction))

(defn fire
  "Raise an event to any reactors whose event-pred returns true for
  `event-id`. The `event-id` and `event-data`, if specified, are passed to
  the reactor."
  ([event-id]
     (fire event-id nil))
  ([event-id event-data]
     (let [matching-reactions (filter (fn [[{event-pred :event-pred} run-count]]
                                        (event-pred event-id))
                                      @reactions)]
       (doseq [[reaction run-count] matching-reactions]
         (let [{:keys [max-count reactor]} reaction
               run-count (inc run-count)]
           (reactor event-id event-data)
           (if (and max-count
                    (<= max-count run-count))
             (delete-reaction reaction)
             (swap! reactions assoc reaction run-count)))))))

(comment

  (require '[cljs.repl :as repl])
  (require '[cljs.repl.rhino :as rhino])
  (repl/repl (rhino/repl-env))

  (do
    (let [recorded-reactions (atom [])
          reaction (react-to #{:do-something} #(swap! recorded-reactions conj [%1 %2]))]
      ;; Did we get a reaction back?
      (assert reaction)
      (fire :do-something)
      ;; Did the reactor catch the event?
      (assert (= [[:do-something nil]] @recorded-reactions))
      (fire :do-something)
      ;; Did the reactor catch the event a second time?
      (assert (= [[:do-something nil] [:do-something nil]] @recorded-reactions))
      (fire :something-else)
      ;; Did we ignore events we're not reacting to?
      (assert (= [[:do-something nil] [:do-something nil]] @recorded-reactions))
      (reset! recorded-reactions [])
      (fire :do-something 17)
      ;; Does event data arrive intact?
      (assert (= [[:do-something 17]] @recorded-reactions))
      (reset! recorded-reactions [])
      (delete-reaction reaction)
      (fire :do-something 17)
      ;; Does deleting a reaction cause us to stop reacting?
      (assert (= [] @recorded-reactions)))
    (let [recorded-reactions (atom #{})
          reaction-once (react-to 1 #{:do-something}
                                  #(swap! recorded-reactions conj [1 %1 %2]))
          reaction-twice (react-to 2 #{:do-something}
                                   #(swap! recorded-reactions conj [2 %1 %2]))]
      (fire :do-something 1)
      ;; Did both reactions react?
      (assert (= #{[1 :do-something 1] [2 :do-something 1]} @recorded-reactions))
      (fire :do-something 2)
      ;; Did only the second reaction react?
      (assert (= #{[1 :do-something 1] [2 :do-something 1] [2 :do-something 2]}
                 @recorded-reactions))
      (fire :do-something 3)
      ;; Did nothing change?
      (assert (= #{[1 :do-something 1] [2 :do-something 1] [2 :do-something 2]}
                 @recorded-reactions)))
    true
    )
  )
