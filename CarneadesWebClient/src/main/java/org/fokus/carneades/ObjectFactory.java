
package org.fokus.carneades;

import javax.xml.bind.JAXBElement;
import javax.xml.bind.annotation.XmlElementDecl;
import javax.xml.bind.annotation.XmlRegistry;
import javax.xml.namespace.QName;


/**
 * This object contains factory methods for each 
 * Java content interface and Java element interface 
 * generated in the org.fokus.carneades package. 
 * <p>An ObjectFactory allows you to programatically 
 * construct new instances of the Java representation 
 * for XML content. The Java representation of XML 
 * content can consist of schema derived interfaces 
 * and classes representing the binding of schema 
 * type definitions, element declarations and model 
 * groups.  Factory methods for each of these are 
 * provided in this class.
 * 
 */
@XmlRegistry
public class ObjectFactory {

    private final static QName _AskEngine_QNAME = new QName("http://carneades.fokus.org/", "askEngine");
    private final static QName _AskEngineResponse_QNAME = new QName("http://carneades.fokus.org/", "askEngineResponse");

    /**
     * Create a new ObjectFactory that can be used to create new instances of schema derived classes for package: org.fokus.carneades
     * 
     */
    public ObjectFactory() {
    }

    /**
     * Create an instance of {@link AskEngine }
     * 
     */
    public AskEngine createAskEngine() {
        return new AskEngine();
    }

    /**
     * Create an instance of {@link AskEngineResponse }
     * 
     */
    public AskEngineResponse createAskEngineResponse() {
        return new AskEngineResponse();
    }

    /**
     * Create an instance of {@link CarneadesMessage }
     * 
     */
    public CarneadesMessage createCarneadesMessage() {
        return new CarneadesMessage();
    }

    /**
     * Create an instance of {@link Statement }
     * 
     */
    public Statement createStatement() {
        return new Statement();
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link AskEngine }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://carneades.fokus.org/", name = "askEngine")
    public JAXBElement<AskEngine> createAskEngine(AskEngine value) {
        return new JAXBElement<AskEngine>(_AskEngine_QNAME, AskEngine.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link AskEngineResponse }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://carneades.fokus.org/", name = "askEngineResponse")
    public JAXBElement<AskEngineResponse> createAskEngineResponse(AskEngineResponse value) {
        return new JAXBElement<AskEngineResponse>(_AskEngineResponse_QNAME, AskEngineResponse.class, null, value);
    }

}
