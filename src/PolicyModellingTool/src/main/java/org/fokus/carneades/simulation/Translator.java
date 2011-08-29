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


// TODO : reconsider if it is better to use OWL annotations for this purpose
/**
 * 
 * This class translates formal question statements coming the Carneades engine
 * to question objects that can be easily send to the web client including:
 * 
 *  - natural language representation in any language
 *  - type of question (text, number, date, radio, list, ...)
 *  - category
 *  - hint text explaining why this question is relevant for the policy
 *  - other questions that should be shown on the same page connected to the initial question
 *  - possible answers from which the user will choose
 * 
 * This mapping is defined in an XML file which uses the "IMPACT Translations" schema.
 *
 * @author stb
 */
public class Translator {
    
    private Map<String, Translation> translations;
    
    // element names used for XMl parsing
    private final String PRED_ELEM = "predicate";
    private final String STMTS_ELEM = "statements";
    private final String QUESTION_ELEM = "question";
    private final String FORMAT_ELEM = "format";
    private final String ARGS_ELEM = "args";
    private final String ARG_ELEM = "arg";
    private final String HINT_ELEM = "hint";
    private final String CAT_ELEM = "category";
    private final String QREFS_ELEM = "qrefs";
    private final String QREF_ELEM = "qref";
    private final String TEXT_ELEM = "text";
    private final String TRANSL_ELEM = "translations";
    private final String ANSWERS_ELEM = "answers";
    
    private final String LANG_ATTR = "lang";
    private final String PRED_ATTR = "pred";
    private final String ARGS_ATTR = "args";
    private final String ARG_ATTR = "arg";
    private final String NR_ATTR = "nr";
    private final String TYPE_ATTR = "type";
    
    private int qid = 0;
    
    private static final Logger log = LoggerFactory.getLogger(Translator.class);
    
    /**
     * 
     * reads the xml file and constructs internal representation for easy access
     * 
     * @param filename xml file containing the mapping
     */
    public Translator(String filename) {
    
        try {
            // loading translation.xml
            DocumentBuilder builder = DocumentBuilderFactory.newInstance().newDocumentBuilder();
            Document document = builder.parse(new File(filename));
            
            // putting translations into map
            this.translations = new HashMap<String, Translation>();
            NodeList predicates = document.getElementsByTagName(PRED_ELEM);            
            for(int i=0; i<predicates.getLength(); i++) {
                
                Node pNode = predicates.item(i);                
                
                // getting atrributes of predicate
                NamedNodeMap attr = pNode.getAttributes();
                String pred = attr.getNamedItem(PRED_ATTR).getNodeValue();
                Node argsAttr = attr.getNamedItem(ARGS_ATTR);
                String argVal = argsAttr.getNodeValue();
                int args = Integer.parseInt(argVal);
                
                // handling statements                
                Node stmtsNode = findChild(this.STMTS_ELEM, pNode);
                Map<String, FormatText> text = handleFormat(stmtsNode);
                
                // handling questions
                Map<Integer, StructuredQuestion> questions = handleQuestions(pNode);
                for(StructuredQuestion q : questions.values()) {
                    log.info("qid: "+Integer.toString(q.getId()));
                }
                
                // adding translation
                Translation trl = new Translation(pred, args, questions, text);
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
    
    /**
     * 
     * Get the formatted string in natural language representing the formal statement
     * 
     * @param stmt the statement to be represented
     * @param language language
     * @return string in natural language representing the statement
     */
    public String getStatementText(Statement stmt, String language) {

        String text = "undefined";
        String pred = stmt.getPredicate();
        Translation t = this.translations.get(pred);
        Map<String, FormatText> textMap = t.getText();
        FormatText form = textMap.get(language);
        if(form == null) {
            String fallbackLang = textMap.keySet().iterator().next();
            form = textMap.get(fallbackLang);
            log.warn("could not find language for question: "+language);
            log.warn("using language: "+fallbackLang);
            text = GoogleTranslate.translate(form.format(stmt.getArgs()), fallbackLang, language);
        } else {
            text = form.format(stmt.getArgs());
        }
        log.info("getStatementText: "+text);
        return text;
    }
    
    /**
     * 
     * Get the list of structured question objects for a formal question statement
     * 
     * @param stmt the question statement
     * @return list of structured questions connected to the formal statement
     */
    public List<StructuredQuestion> getStructuredQuestions(Statement stmt) {
        
        List<StructuredQuestion> questions = new ArrayList<StructuredQuestion>();
        
        // getting translation
        String pred = stmt.getPredicate();
        Translation translation = this.translations.get(pred);         
        List<String> args = stmt.getArgs();
        
        // getting question
        log.info("getQuestion: "+stmt.toString());
        StructuredQuestion q = getQuestion(translation, args);
        q.setStatement(stmt);
        questions.add(q);
        
        // getting linked questions
        List<QuestionRef> refs = q.getRefs();
        if (refs != null) {
            for(QuestionRef ref : refs) {
                String refPred = ref.getPred();
                Integer refArg = ref.getArg();
                List<Integer> refArgs = ref.getArgs();
                // get translation for ref
                Translation refTranslation = this.translations.get(refPred);
                // get right question for ref
                StructuredQuestion refQuestion = refTranslation.getQuestions().get(refArg);
                // construct statement for ref
                Statement refStmt = new Statement();
                refStmt.setPredicate(refPred);
                String[] refArgsArray = new String[refTranslation.getArgs()];
                int c = 0;
                System.out.println("stmt : "+stmt);
                for(Integer a : refArgs) {
                    System.out.println("a    : "+a);
                    System.out.println("c    : "+c);
                    System.out.println("stmt : "+stmt.getArgs().get(a));
                    refArgsArray[c] = stmt.getArgs().get(a);
                    c++;
                }
                refArgsArray[refArg] = "?x";
                refStmt.setArgs(Arrays.asList(refArgsArray));
                System.out.println("refstmt: "+refStmt);
                refQuestion.setStatement(refStmt);
                // add ref
                questions.add(refQuestion);
            }
        }
        
        
        return questions;
    }

    private Map<String, FormatText> handleFormat(Node node) {
        Map<String, FormatText> text = new HashMap<String, FormatText>();
        List<Node> stmtFormatNodes = findChildren(this.FORMAT_ELEM, node);        
        for(Node stmtFormatNode : stmtFormatNodes) {
            // language
            NamedNodeMap stmtFormatAttr = stmtFormatNode.getAttributes();
            String lang = stmtFormatAttr.getNamedItem(LANG_ATTR).getNodeValue();
            // text & args 
            String t = findChild(this.TEXT_ELEM, stmtFormatNode).getTextContent();
            List<Integer> args = handleArgs(stmtFormatNode);
            FormatText ft = new FormatText(t, args);
            text.put(lang, ft);
        }
        return text;       
    }

    private List<Integer> handleArgs(Node node) {
        Node argsNode = findChild(this.ARGS_ELEM, node);
        List<Node> argNodes = findChildren(this.ARG_ELEM, argsNode);
        List<Integer> args = new ArrayList<Integer>();
        for(Node argNode : argNodes) {
            NamedNodeMap argAttr = argNode.getAttributes();
            String nrString = argAttr.getNamedItem(this.NR_ATTR).getNodeValue();
            args.add(Integer.parseInt(nrString));
        }
        return args;
    }

    private Map<Integer, StructuredQuestion> handleQuestions(Node predicateNode) {        
        Map<Integer, StructuredQuestion> questions = new HashMap<Integer, StructuredQuestion>();
        
        List<Node> questionNodes = findChildren(this.QUESTION_ELEM, predicateNode);
        
        for(Node questionNode : questionNodes) {
            // attributes
            NamedNodeMap qAttr = questionNode.getAttributes();
            int arg = Integer.parseInt(qAttr.getNamedItem(this.ARG_ATTR).getNodeValue());
            String type = qAttr.getNamedItem(this.TYPE_ATTR).getNodeValue();
            
            // format+
            Map<String, FormatText> text = handleFormat(questionNode);
            
            // hint
            String hint = findChild(this.HINT_ELEM, questionNode).getTextContent();
            
            // category
            String category = findChild(this.CAT_ELEM, questionNode).getTextContent();
            
            // answers?
            List<String> answers = handleAnswers(questionNode);
            
            // questionrefs?
            List<QuestionRef> refs = handleRefs(questionNode);
            
            StructuredQuestion q = new StructuredQuestion(arg, type, text, hint, category, answers, refs);
            q.setId(qid++);
            questions.put(arg, q);
        }
        
        return questions;        
    }

    private List<String> handleAnswers(Node node) {
        List<String> answers = new ArrayList<String>();        
        Node answersNode = findChild(this.ANSWERS_ELEM, node);
        if(answersNode != null) {
            List<Node> textNodes = findChildren(this.TEXT_ELEM, answersNode);
            for(Node textNode : textNodes) {
                answers.add(textNode.getTextContent());
            }
        }        
        return answers;
    }

    private List<QuestionRef> handleRefs(Node node) {
        List<QuestionRef> refs = new ArrayList<QuestionRef>();
        
        Node qrefsNode = findChild(this.QREFS_ELEM, node);
        if(qrefsNode != null) {
            List<Node> qrefNodes = findChildren(this.QREF_ELEM, qrefsNode);
            for(Node refNode : qrefNodes) {
                // attributes
                NamedNodeMap refAttr = refNode.getAttributes();
                String pred = refAttr.getNamedItem(this.PRED_ATTR).getNodeValue();
                Integer arg = Integer.parseInt(refAttr.getNamedItem(this.ARG_ATTR).getNodeValue());
                // args
                List<Integer> args = handleArgs(refNode);
                refs.add(new QuestionRef(pred, arg, args));
            }
        }
        
        return refs;
    }

    private StructuredQuestion getQuestion(Translation translation, List<String> args) {
        int varPos = 0;
        for(String arg : args) {
            if(arg.startsWith("?")) {
                return translation.getQuestions().get(varPos);                                 
            }
            varPos++;
        }        
        return translation.getQuestions().get(translation.getArgs()-1);
    }

        

}
