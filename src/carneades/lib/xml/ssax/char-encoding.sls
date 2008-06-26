;;;
;;             Character-encoding module
;;
;; This module deals with particular character-encoding issues such as
;; conversions between characters and their ASCII or UCS2 codes, Scheme
;; representations of "Carriage Return" (CR), "tabulation" (TAB) and
;; other control characters.
;;
;; This module by necessity is platform-specific as character encoding
;; issues are hardly addressed in R5RS. For example, the result of
;; char->integer is generally not an ASCII code of an integer (although
;; it is, on many Scheme systems, with the important exception of
;; Scheme48 and SCSH). The level of support for character sets other
;; than ASCII varies widely among Scheme systems.
;;
;; This file collects various character-encoding functions that are
;; necessary for the SSAX XML parser. The functions are of general use
;; and scope.
;;

;;;
;; Note this whole thing needs to be reviewed with regard to the new
;; standard R6RS libraries for Chars, as well as unicode support. RPR
;;;

(library
  (carneades lib xml ssax char-encoding)
  
  (export
   ascii->char
   ucscode->char
   char-return)
  
  (import (rnrs base))
  
;;;  
  ;;     ascii->char INT -> CHAR
  ;; return a character whose ASCII code is INT
  ;; Note, because ascii->char is injective (there are more characters than
  ;; ASCII characters), the inverse transformation is not defined.
  
  (define ascii->char integer->char)
  
;;;
  ;;     ucscode->char INT -> CHAR
  ;; Return a character whose UCS (ISO/IEC 10646) code is INT
  ;; Note
  ;; This function is required for processing of XML character entities:
  ;; According to Section "4.1 Character and Entity References"
  ;; of the XML Recommendation:
  ;;  "[Definition: A character reference refers to a specific character
  ;;   in the ISO/IEC 10646 character set, for example one not directly
  ;;   accessible from available input devices.]"
  
  (define (ucscode->char code)
    (integer->char code))

;;;  
  ;; Commonly used control characters
  
  (define char-return
    (ascii->char 13))
  
  (define char-tab
    (ascii->char 9))
  
  (define char-newline
    (ascii->char 10)) ;; a.k.a. #\newline, per R5RS
  
  (define char-space
    (ascii->char 32)))
