(import
 (rnrs base)
 (only (rnrs io simple)
       open-input-file close-input-port
       display)
 (ssax ssax-code))

(define tiny
  "/code/editio/bravais/trunk/src/scheme/w3c/xml/ssax/tiny.xml")

(define big
  "/code/simplifyEAI/EAIXML/EAIXML/oagis/v9/message/bod/ProcessPurchaseOrder.xsd")

(define ns (list
            '(b ."http://bravais.org")))

(define parse-big
  (lambda ()
    (let ((ip (open-input-file big)))
      (let ((doc (xml->sxml ip ns)))
        (close-input-port ip)
        doc))))

(define doc (parse-big))