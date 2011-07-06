/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.fokus.carneades.lkif;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import org.fokus.carneades.api.Statement;
import org.w3c.dom.Document;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

/**
 * 
 * Utility class to handle some LKIF parsing
 *
 * @author stb
 */
// TODO : LKIFHelper may be obsolete?
public class LKIFHelper {
    
    /**
     * 
     * extracts the statements of an argument graph in an LKIF file
     * 
     * @param lkifPath path to lkif file containing an argument graph
     * @return  map of statements in the graph (id -> statement)
     */
    public static Map<String, Statement> getStmtIDs(String lkifPath) {
        
        Map<String, Statement> stmts = new HashMap<String, Statement>();
        
        try {
        
            DocumentBuilder builder = DocumentBuilderFactory.newInstance().newDocumentBuilder();
            Document document = builder.parse(lkifPath);
            NodeList stmtNodes = document.getElementsByTagName("statement");
            
            for(int i=0; i<stmtNodes.getLength(); i++) {
                Node stmtNode = stmtNodes.item(i);
                NamedNodeMap stmtAttr = stmtNode.getAttributes();
                String stmtID = stmtAttr.getNamedItem("id").getNodeValue();
                Statement stmt = getStatement(stmtNode);
                stmts.put(stmtID, stmt);
            }
            
        } catch (Exception e) {
            e.printStackTrace();
        } finally {
            return stmts;
        }
        
    }
    
    private static Node findChild(String c, Node p) {
        NodeList l = p.getChildNodes();
        for(int i=0; i<l.getLength(); i++) {
            Node n = l.item(i);
            if(n.getNodeName().equals(c))
                return n;
        }
        return null;
    }

    private static Statement getStatement(Node stmtNode) {
        
        Node sNode = findChild("s", stmtNode);
        
        NamedNodeMap sAttr = sNode.getAttributes();
        String pred = sAttr.getNamedItem("pred").getNodeValue();
        
        NodeList children = sNode.getChildNodes();
        List<String> args = new ArrayList<String>();
        for(int i=0; i<children.getLength(); i++) {
            args.add(getArg(children.item(i)));
        }
        
        Statement stmt = new Statement();
        stmt.setArgs(args);
        stmt.setPredicate(pred);
        
        return stmt;
                
    }

    private static String getArg(Node argNode) {
        
        if(argNode.getNodeName().equals("c")) {
            return argNode.getTextContent();
        } else {
            System.out.println("LKIFHelper.getArg() : unhandled Node : "+argNode.getNodeName());
            return "undefined";
        }
        
    }
    
}
