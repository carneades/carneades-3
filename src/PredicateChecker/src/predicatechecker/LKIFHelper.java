/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package predicatechecker;

import java.io.File;
import java.util.HashSet;
import java.util.Set;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import org.w3c.dom.Document;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

/**
 *
 * @author stb
 */
public class LKIFHelper {

    public static Set<String> getPredicatesFromLKIF(File f) {
        Set<String> predicates = new HashSet<String>();

        try {

            // loading translation.xml
            DocumentBuilder builder = DocumentBuilderFactory.newInstance().newDocumentBuilder();
            Document document = builder.parse(f);

            NodeList predicateNodes = document.getElementsByTagName("s");

            for(int i=0; i<predicateNodes.getLength(); i++) {
                Node sNode = predicateNodes.item(i);
                NamedNodeMap sAttr = sNode.getAttributes();
                String pred = sAttr.getNamedItem("pred").getNodeValue();
                predicates.add(pred);
            }

        } catch (Exception e) {

            e.printStackTrace();

        } finally {
            return predicates;
        }
    }

}
