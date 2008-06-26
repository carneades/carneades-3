
(define files
  (list "char-encoding.scm" "input-parse.scm"
        "ssax-code.scm" "util.scm"
        "common.scm" "look-for-str.scm"
        "ssax-parser.scm"))

(for-each compile-file files)
