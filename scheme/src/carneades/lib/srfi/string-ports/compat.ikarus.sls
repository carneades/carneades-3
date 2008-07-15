#!r6rs

(library (carneades lib srfi string-ports compat)
  (export
    open-output-string get-output-string)
  (import
    (only (ikarus) open-output-string get-output-string)))
