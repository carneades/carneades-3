(ns catb.i18n)

(defn i18n
  [k]
  (js/jQuery.i18n.prop k))