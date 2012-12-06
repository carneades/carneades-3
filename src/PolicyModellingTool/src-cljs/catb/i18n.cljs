;;; Copyright (c) 2012 Fraunhofer Gesellschaft
;;; Licensed under the EUPL V.1.1

(ns catb.i18n)

(defn i18n
  [k]
  (js/jQuery.i18n.prop k))