/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.fokus.carneades.simulation;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import org.fokus.carneades.api.Statement;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.w3c.dom.Document;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

/**
 *
 * @author stb
 */
public class Translator {
    
    private Map<String, Translation> translations = new HashMap<String, Translation>();
    
    private static final String PRED_ELEM = "predicate";
    private static final String STMTS_ELEM = "statements";
    private static final String QUESTION_ELEM = "question";
    private static final String FORMAT_ELEM = "format";
    private static final String ARGS_ELEM = "args";
    private static final String ARG_ELEM = "arg";
    private static final String HINT_ELEM = "hint";
    private static final String CAT_ELEM = "category";
    private static final String QREFS_ELEM = "qrefs";
    private static final String QREF_ELEM = "qref";
    public static final String TEXT_ELEM = "text";
    
    private static final String LANG_ATTR = "lang";
    private static final String PRED_ATTR = "pred";
    private static final String ARGS_ATTR = "args";
    private static final String ARG_ATTR = "arg";
    private static final String NR_ATTR = "nr";
    private static final String TYPE_ATTR = "type";
    
    private static final Logger log = LoggerFactory.getLogger(Translator.class);
    
    public Translator(String filename) {
    
        try {
            // loading translation.xml
            DocumentBuilder builder = DocumentBuilderFactory.newInstance().newDocumentBuilder();
            Document document = builder.parse(new File(filename));
            
            // putting translations into map
            NodeList predicates = document.getElementsByTagName(PRED_ELEM);
            int qid = 0;
            for(int i=0; i<predicates.getLength(); i++) {
                Node pNode = predicates.item(i);
                
                // getting atrributes of predicate
                NamedNodeMap attr = pNode.getAttributes();
                String pred = attr.getNamedItem(PRED_ATTR).getNodeValue();
                Node argsAttr = attr.getNamedItem(ARGS_ATTR);
                String argVal = argsAttr.getNodeValue();
                int args = Integer.parseInt(argVal);
                
                // handling children
                Map<Integer, Question> questions = new HashMap<Integer, Question>();
                Map<Question, List<QuestionRef>> questionRefs = new HashMap<Question, List<QuestionRef>>();               
                
                Map<String, FormatText> text = new HashMap<String, FormatText>();
                NodeList predChildren = pNode.getChildNodes();
                for(int j=0; j<predChildren.getLength(); j++) {
                    Node n = predChildren.item(j);
                    String nName = n.getNodeName();
                    NamedNodeMap nodeAttr = n.getAttributes();
                    if(STMTS_ELEM.equals(nName)) {
                        // adding statement text                        
                        String lang = nodeAttr.getNamedItem(LANG_ATTR).getNodeValue();
                        // NodeList stmtChildren = n.getChildNodes();
                        Node formatNode = findChild(FORMAT_ELEM, n);
                        String format = formatNode.getTextContent();
                        Node argsNode = findChild(ARGS_ELEM, n);
                        List<Node> argsList = findChildren(ARG_ELEM, argsNode);
                        int argsLength = argsList.size();
                        Integer[] formatArgs = new Integer[argsLength];
                        for(int k=0; k<argsLength; k++) {
                            formatArgs[k] = Integer.parseInt(argsList.get(k).getAttributes().getNamedItem(NR_ATTR).getNodeValue());
                        }
                        FormatText ft = new FormatText(format, formatArgs);
                        text.put(lang, ft);
                    } else if (QUESTION_ELEM.equals(nName)) {
                        // adding Question
                        // TODO : handle languages in questions
                        String lang = nodeAttr.getNamedItem(LANG_ATTR).getNodeValue();
                        Integer arg = Integer.parseInt(nodeAttr.getNamedItem(ARG_ATTR).getNodeValue()); 
                        String type = nodeAttr.getNamedItem(TYPE_ATTR).getNodeValue();
                        Node formatNode = findChild(FORMAT_ELEM, n);                        
                        Question q = new Question();       
                        // id
                        q.setId(qid);
                        qid++;
                        // type
                        q.setType(type);
                        // question
                        String format = formatNode.getTextContent();
                        Node argsNode = findChild(ARGS_ELEM, n);
                        List<Node> argsList = findChildren(ARG_ELEM, argsNode);
                        int argsLength = argsList.size();
                        Integer[] formatArgs = new Integer[argsLength];
                        for(int k=0; k<argsLength; k++) {
                            formatArgs[k] = Integer.parseInt(argsList.get(k).getAttributes().getNamedItem(NR_ATTR).getNodeValue());
                        }
                        FormatText ft = new FormatText(format, formatArgs);
                        q.setQuestion(ft);
                        // hint
                        String hint = findChild(HINT_ELEM,n).getTextContent();
                        q.setHint(hint);
                        // statement
                        /*Statement stmt = new Statement();
                        stmt.setPredicate(pred);
                        for(int k=0; k<args; k++) {
                            stmt.getArgs().add("?x"+Integer.toString(k));
                        }
                        q.setStatement(stmt);*/
                        // category
                        String category = findChild(CAT_ELEM, n).getTextContent();
                        // TODO : add possible answers to question
                        q.setCategory(category);
                        // question refs
                        Node refsNode = findChild(QREFS_ELEM, n);
                        if(refsNode != null) {
                            List<Node> refNodes = findChildren(QREF_ELEM, refsNode);
                            List<QuestionRef> refs = new ArrayList<QuestionRef>();
                            for(Node rNode : refNodes) {
                                NamedNodeMap refAttr = rNode.getAttributes();
                                String refPred = refAttr.getNamedItem(PRED_ATTR).getNodeValue();
                                Integer refArg = Integer.parseInt(refAttr.getNamedItem(ARG_ATTR).getNodeValue());
                                List<Node> refArgNodes = findChildren(ARG_ELEM, findChild(ARGS_ELEM, rNode));
                                List<Integer> refArgs  = new ArrayList<Integer>();
                                for(Node refArgNode : refArgNodes) {
                                    Integer refArgNr = Integer.parseInt(refArgNode.getAttributes().getNamedItem(NR_ATTR).getNodeValue());
                                    refArgs.add(refArgNr);
                                }
                                QuestionRef ref = new QuestionRef(refPred, refArg, refArgs);
                                refs.add(ref);
                            }
                            questionRefs.put(q, refs);
                        }
                        // add question to question map
                        questions.put(arg, q);
                    } else {
                        log.info("unknown child node in "+PRED_ELEM+": "+nName);
                    }
                }
                
                Translation trl = new Translation(pred, args, questions, questionRefs, text);
                this.translations.put(pred, trl);
            }
            
            
        } catch (Exception e) {  
            e.printStackTrace();
            log.error("something happened while creating translator:" + e);            
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

    private static List<Node> findChildren(String c, Node p) {
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
    
    public String getStatementText(Statement stmt, String language) {
                
        String pred = stmt.getPredicate();
        Translation t = this.translations.get(pred);
        FormatText text = t.getText().get(language);
        log.info("getStatementText: "+stmt.toString());
        return text.format(stmt.getArgs());
    }
    
    
    public List<Question> getQuestions(Statement stmt, String language) {
        
        List<Question> questions = new ArrayList<Question>();
        
        // getting translation
        String pred = stmt.getPredicate();
        Translation translation = this.translations.get(pred);         
        List<String> args = stmt.getArgs();
        
        // getting question
        Question q = null;   
        int varPos = 0;
        for(String arg : args) {
            if(arg.startsWith("?")) {
                q = translation.getQuestions().get(varPos);                 
            }
            varPos++;
        }
        log.info("getQuestion: "+stmt.toString());
        q.setStatement(stmt);
        questions.add(q);
        
        // getting linked questions
        List<QuestionRef> refs = translation.getQuestionRefs().get(q);
        if (refs != null) {
            for(QuestionRef ref : refs) {
                String refPred = ref.getPred();
                Integer refArg = ref.getArg();
                List<Integer> refArgs = ref.getArgs();
                // get translation for ref
                Translation refTranslation = this.translations.get(refPred);
                // get right question for ref
                Question refQuestion = refTranslation.getQuestions().get(refArg);
                // construct statement for ref
                Statement refStmt = new Statement();
                refStmt.setPredicate(refPred);
                String[] refArgsArray = new String[refTranslation.getArgs()];
                int c = 0;
                for(Integer a : refArgs) {
                    refArgsArray[c] = stmt.getArgs().get(a);
                    c++;
                }
                refArgsArray[refArg] = "?x";
                refStmt.setArgs(Arrays.asList(refArgsArray));
                refQuestion.setStatement(refStmt);
                // add ref
                questions.add(refQuestion);
            }
        }
        
        
        return questions;
    }

}
