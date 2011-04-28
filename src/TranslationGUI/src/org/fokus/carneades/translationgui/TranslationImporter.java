/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.fokus.carneades.translationgui;

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
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
public class TranslationImporter {
    
    public static List<Predicate> importTranslations(File translationFile) {
        
        List<Predicate> predicates = new ArrayList<Predicate>();
        
        try {
            
            // loading translation.xml
            DocumentBuilder builder = DocumentBuilderFactory.newInstance().newDocumentBuilder();
            Document document = builder.parse(translationFile);
            
            NodeList predicateNodes = document.getElementsByTagName(XMLHelper.PRED_ELEM);
            for(int predI=0; predI<predicateNodes.getLength(); predI++) {
                
                Node predicateNode = predicateNodes.item(predI);
                
                // handle predicate attributes
                NamedNodeMap predAttributes = predicateNode.getAttributes();
                String predName = predAttributes.getNamedItem(XMLHelper.PRED_ATTR).getNodeValue();
                Integer predArgs = Integer.parseInt(predAttributes.getNamedItem(XMLHelper.ARGS_ATTR).getNodeValue());
                
                // handle predicate statement
                Node predStmtsNode = XMLHelper.findChild(XMLHelper.STMST_ELEM, predicateNode);
                Map<String, FormatText> predStmts = importFormats(predStmtsNode);
                
                // handle predicate questions
                List<Node> predQuestionNodes = XMLHelper.findChildren(XMLHelper.QUESTION_ELEM, predicateNode);
                List<Question> questions = new ArrayList<Question>();
                for(Node questionNode : predQuestionNodes) {
                    questions.add(importQuestion(questionNode));
                }
                
                // add predicate
                Predicate predicate = new Predicate(predName, predArgs, predStmts, questions);
                predicates.add(predicate);
                
            }
            
        } catch(Exception e) {            
            // e.printStackTrace();            
            System.out.println(e.getMessage());
        }
        
        return predicates;
        
    }

    private static Map<String, FormatText> importFormats(Node stmtNode) {
        
        List<Node> formatNodes = XMLHelper.findChildren(XMLHelper.FORMAT_ELEM, stmtNode);
        Map<String, FormatText> formatMap = new HashMap<String, FormatText>();
        
        for(Node formatNode : formatNodes) {
            
            // attributes
            NamedNodeMap formatAttributes = formatNode.getAttributes();
            String formatLang = formatAttributes.getNamedItem(XMLHelper.LANG_ATTR).getNodeValue();            
            // format text
            FormatText formatText = importFormatText(formatNode);
            // add to map
            formatMap.put(formatLang, formatText);
            
        }
        
        return formatMap;
    }
    
    

    private static Question importQuestion(Node questionNode) {
        
        // attributes
        NamedNodeMap qAttributes = questionNode.getAttributes();
        int qArg = Integer.parseInt(qAttributes.getNamedItem(XMLHelper.ARG_ATTR).getNodeValue());
        String qType = qAttributes.getNamedItem(XMLHelper.TYPE_ATTR).getNodeValue();
        // format text
        Map<String, FormatText> questionFormats = importFormats(questionNode);
        // hint
        String hint = XMLHelper.findChild(XMLHelper.HINT_ELEM, questionNode).getTextContent();
        // categroy
        String category = XMLHelper.findChild(XMLHelper.CAT_ELEM, questionNode).getTextContent();
        // answers
        List<String> answers = importAnswers(questionNode);
        // question refs
        List<QRef> refs = importQRefs(questionNode);
        
        Question q = new Question(qArg, qType, hint, category, questionFormats, answers, refs);
        
        return q;
    }

    private static FormatText importFormatText(Node stmtNode) {
        
        String text = XMLHelper.findChild(XMLHelper.TEXT_ELEM, stmtNode).getTextContent();
        List<Integer> args = importArgs(stmtNode);
        
        FormatText formText = new FormatText(text, args);
        
        return formText;
    }

    private static List<Integer> importArgs(Node parentNode) {
        Node argsNode = XMLHelper.findChild(XMLHelper.ARGS_ELEM, parentNode);
        List<Integer> args = new ArrayList<Integer>();
        List<Node> argNodes = XMLHelper.findChildren(XMLHelper.ARG_ELEM, argsNode);
        for(Node argNode : argNodes) {
            NamedNodeMap argAttr = argNode.getAttributes();
            int nr = Integer.parseInt(argAttr.getNamedItem(XMLHelper.NR_ATTR).getNodeValue());
            args.add(nr);
        }
        
        return args;
    }

    private static List<QRef> importQRefs(Node questionNode) {
        
        List<QRef> refs = new ArrayList<QRef>();
        
        Node qrefsNode = XMLHelper.findChild(XMLHelper.QREFS_ELEM, questionNode);
        
        if(qrefsNode != null) {
            List<Node> qrefNodes = XMLHelper.findChildren(XMLHelper.QREF_ELEM, qrefsNode);
            for(Node qrefNode : qrefNodes) {
                // attributes
                NamedNodeMap refAttr = qrefNode.getAttributes();
                String refPred = refAttr.getNamedItem(XMLHelper.PRED_ATTR).getNodeValue();
                int refArg = Integer.parseInt(refAttr.getNamedItem(XMLHelper.ARG_ATTR).getNodeValue());
                // args
                List<Integer> refArgs = importArgs(qrefNode);
                // add ref
                QRef ref = new QRef(refPred, refArg, refArgs);
                refs.add(ref);
            }
        }
        
        return refs;
    }

    private static List importAnswers(Node questionNode) {
        
        List<String> answers = new ArrayList<String>();
        
        Node answersNode = XMLHelper.findChild(XMLHelper.ANSWERS_ELEM, questionNode);
        
        if(answersNode != null) {
            List<Node> textNodes = XMLHelper.findChildren(XMLHelper.TEXT_ELEM, answersNode);
            for(Node textNode : textNodes) {
                String t = textNode.getTextContent();
                answers.add(t);
            }
        }
        
        return answers;
    }
    
}
