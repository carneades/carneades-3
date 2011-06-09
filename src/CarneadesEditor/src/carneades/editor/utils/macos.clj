;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns ^{:doc "Mac OS specify functions"}
  carneades.editor.utils.macos
  (:import (com.apple.mrj MRJApplicationUtils MRJQuitHandler)))

(defn register-quit-handler
  "Registers a listener for the quit menu.
   If the platform is not Mac OS this has no effects "
  [f & args]
  (MRJApplicationUtils/registerQuitHandler
   (reify MRJQuitHandler
     (handleQuit
      [this]
      (apply f args)))))

