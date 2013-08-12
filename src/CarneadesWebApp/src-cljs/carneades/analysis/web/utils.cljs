(ns carneades.analysis.web.utils)

(defn ^:export convert
  [d]
  (js->clj d :keywordize-keys true))
