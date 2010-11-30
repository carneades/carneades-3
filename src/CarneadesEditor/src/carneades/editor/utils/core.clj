;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.editor.utils.core)

(defmacro when-nlet [bindings & body]
  "like when-let but only checks that the result of the test is not nil"
  (when (not= (count bindings) 2)
    (throw (IllegalArgumentException.
            "when-nlet requires an even number of forms in binding vector")))
  (let [form (bindings 0)
        tst (bindings 1)]
    `(let [temp# ~tst]
        (when (not (nil? temp#))
          (let [~form temp#]
            ~@body)))))

(defmacro when-let* [bindings & body]
  "like when-nlet but allows multiple tests in bindings

   (when-let* [a (expr1)
               b (expr2)])

   will expands to:

   (when-nlet [a (expr1)]
      (when-nlet [b expr2]))"
  (when (not (even? (count bindings)))
    (throw (IllegalArgumentException.
            "when-let* requires an even number of forms in binding vector")))
  (let [whenlets (reduce (fn [sexpr bind]
                           (let [form (first bind)
                                 tst (second bind)]
                             (conj sexpr `(when-nlet [~form ~tst]))))
                         ()
                         (partition 2 bindings))
        body (cons 'do body)]
    `(->> ~body ~@whenlets)))

(defmacro with-out-file [pathname & body]
  `(with-open [stream# (java.io.FileWriter. ~pathname)]
     (binding [*out* stream#
               *err* stream#]
       ~@body)))