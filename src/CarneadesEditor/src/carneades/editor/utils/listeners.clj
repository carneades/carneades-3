;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.editor.utils.listeners)

(defmacro gen-listeners-fns [name]
  "Called with (gen-listener-fns name)
   will generate the two following functions.

   (register-name-listener [f args])
   registers a listener which will be called with args as arguments
   We don't use variadic arguments since protocols do not support them.

   (call-name-listeners [& fargs])
   calls each registered listener with (apply f (concat fargs args))
   "
  (let [register-fn-name (symbol (str "register-" name "-listener"))
        listener-var-name (symbol (str "*" name "-listeners*"))
        call-fn-name (symbol (str "call-" name "-listeners"))]
    `(do
       (clojure.contrib.def/defvar- ~listener-var-name (atom ()))
      
       (defn ~register-fn-name
         [~'f ~'args]
         (swap! ~listener-var-name conj {:listener ~'f :args ~'args}))

       (defn ~call-fn-name
         [& ~'fargs]
         (doseq [{:keys [~'listener ~'args]} (deref ~listener-var-name)]
           (apply ~'listener (concat ~'fargs ~'args)))))))
