#!r6rs
(library (carneades srfi private implementation-features)
  (export
    implementation-features)
  (import
    (rnrs base))
  
  (define implementation-features
    '(mzscheme))
)
