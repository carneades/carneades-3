;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.editor.utils.state)

(defn state-call [f state]
  "calls f with the value of the atom state.
   The value returns by f becomes the new state value 
   if it is not nil."
  (when-let [rval (f (deref state))]
    (reset! state rval)))

