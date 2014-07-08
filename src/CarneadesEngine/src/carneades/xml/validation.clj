(ns carneades.xml.validation
  (:import javax.xml.XMLConstants
           org.xml.sax.SAXException
           javax.xml.validation.SchemaFactory
           java.io.File
           java.io.StringReader
           javax.xml.transform.stream.StreamSource))

(defn create-validation-fn
  "Creates a validator from scheme. Return {:right true} or {:left msg}."
  [schema]
  (let [validator (.newValidator
                   (.newSchema
                    (SchemaFactory/newInstance XMLConstants/W3C_XML_SCHEMA_NS_URI)
                    (StreamSource. (File. schema))
                    ))]
    (fn [xmldoc]
      (try
        (.validate validator
                   (StreamSource. (StringReader. xmldoc)))
        {:right true}
        (catch SAXException e {:left (str e)})))))

