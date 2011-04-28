/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.fokus.carneades.translationgui;

import java.util.ArrayList;
import java.util.List;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

/**
 *
 * @author stb
 */
public class XMLHelper {
    
    public static final String PRED_ELEM = "predicate";
    public static final String STMST_ELEM = "statements";
    public static final String QUESTION_ELEM = "question";
    public static final String FORMAT_ELEM = "format";
    public static final String ARGS_ELEM = "args";
    public static final String ARG_ELEM = "arg";
    public static final String HINT_ELEM = "hint";
    public static final String CAT_ELEM = "category";
    public static final String QREFS_ELEM = "qrefs";
    public static final String QREF_ELEM = "qref";
    public static final String TEXT_ELEM = "text";
    public static final String TRANSL_ELEM = "translations";
    public static final String ANSWERS_ELEM = "answers";
    
    public static final String LANG_ATTR = "lang";
    public static final String PRED_ATTR = "pred";
    public static final String ARGS_ATTR = "args";
    public static final String ARG_ATTR = "arg";
    public static final String NR_ATTR = "nr";
    public static final String TYPE_ATTR = "type";
    
    public static Node findChild(String c, Node p) {
        NodeList l = p.getChildNodes();
        for(int i=0; i<l.getLength(); i++) {
            Node n = l.item(i);
            if(n.getNodeName().equals(c))
                return n;
        }
        return null;
    }

    public static List<Node> findChildren(String c, Node p) {
        NodeList l = p.getChildNodes();
        List<Node> r = new ArrayList<Node>();        
        for(int i=0; i<l.getLength(); i++) {
            Node n = l.item(i);
            if(n.getNodeName().equals(c)) {
                r.add(n);
            }

        }
        return r;
    }
    
}
