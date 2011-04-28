/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.fokus.carneades.translationgui;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import org.jdom.Document;
import org.jdom.Element;
import org.jdom.output.Format;
import org.jdom.output.XMLOutputter;

/**
 *
 * @author stb
 */
public class TranslationExporter {
    
    public static void exportTranslations(List<Predicate> predicates, File f) {
        
        // top level element
        Element translations = new Element(XMLHelper.TRANSL_ELEM);
        
        for(Predicate pred : predicates) {
            
            // predicate element
            Element predElem = new Element(XMLHelper.PRED_ELEM);
            translations.addContent(predElem);
            // attributes of predicate element
            predElem.setAttribute(XMLHelper.PRED_ATTR, pred.getPred());
            predElem.setAttribute(XMLHelper.ARGS_ATTR, Integer.toString(pred.getArgs()));     
            // child elements of predicate
            // Statements
            Element stmtsElem = new Element(XMLHelper.STMST_ELEM);
            predElem.addContent(stmtsElem);
            Map<String, FormatText> formatTexts = pred.getFormatTextMap();
            handleFormatTexts(stmtsElem, formatTexts);
            // Question*
            List<Question> questions = pred.getQuestions();
            handleQuestions(predElem, questions);
            
        }
        
        Document doc = new Document(translations);
        XMLOutputter output = new XMLOutputter(Format.getPrettyFormat());
        try {
            FileWriter fWriter = new FileWriter(f);
            output.output(doc, fWriter);
            fWriter.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
        
    }

    private static void handleQuestions(Element predElem, List<Question> questions) {
        
        for(Question question : questions) {
            
            // new question element
            Element questionElem = new Element(XMLHelper.QUESTION_ELEM);
            predElem.addContent(questionElem);
            // attributes of question element
            questionElem.setAttribute(XMLHelper.ARG_ATTR, Integer.toString(question.getArg()));
            questionElem.setAttribute(XMLHelper.TYPE_ATTR, question.getType());
            // child elements of question
            // Format+
            Map<String, FormatText> formatTextMap = question.getFormatTextMap();
            handleFormatTexts(questionElem, formatTextMap);
            // Hint
            String hint = question.getHint();
            Element hintElem = new Element(XMLHelper.HINT_ELEM);
            questionElem.addContent(hintElem);
            hintElem.addContent(hint);
            // Category
            String category = question.getCategory();
            Element catElem = new Element(XMLHelper.CAT_ELEM);
            questionElem.addContent(catElem);
            catElem.addContent(category);
            // Answers
            List<String> answers = question.getAnswers();
            if(!answers.isEmpty()) {
                Element answersElement = new Element(XMLHelper.ANSWERS_ELEM);
                questionElem.addContent(answersElement);
                handleAnswers(answersElement, answers);
            }
            // QuestionRefs?
            List<QRef> refs = question.getRefs();
            if(!refs.isEmpty()) {
                // Questionrefs
                Element qrefsElem = new Element(XMLHelper.QREFS_ELEM);
                questionElem.addContent(qrefsElem);
                handleQRefs(qrefsElem, refs);
            }
        }
        
    }

    private static void handleFormatTexts(Element elem, Map<String, FormatText> formatTexts) {
        
        Set<String> languages = formatTexts.keySet();
        Iterator<String> iter = languages.iterator();
        // Format+
        while(iter.hasNext()) {
            String lang = iter.next();
            FormatText formText = formatTexts.get(lang) ;
            // new format element
            Element formatElem = new Element(XMLHelper.FORMAT_ELEM);
            elem.addContent(formatElem);
            // attributes of format element
            formatElem.setAttribute(XMLHelper.LANG_ATTR, lang);
            // child elements
            // Text
            String text = formText.getText();
            Element textElem = new Element(XMLHelper.TEXT_ELEM);
            formatElem.addContent(textElem);
            textElem.addContent(text);
            // Args
            List<Integer> argOrder = formText.getArgOder();
            handleArgs(formatElem, argOrder);
        }
        
    }

    private static void handleQRefs(Element qrefsElem, List<QRef> refs) {
        
        // Questionrefs+
        for(QRef ref : refs) {
            // Questionref
            Element qrefElem = new Element(XMLHelper.QREF_ELEM);
            qrefsElem.addContent(qrefElem);
            // attributes
            qrefElem.setAttribute(XMLHelper.PRED_ATTR, ref.getPred());
            qrefElem.setAttribute(XMLHelper.ARG_ATTR, Integer.toString(ref.getArg()));
            // Args
            List<Integer> argOrder = ref.getArgOrder();
            handleArgs(qrefElem, argOrder);
        }
                
    }

    private static void handleArgs(Element elem, List<Integer> argOrder) {
        
        // Args
        Element argsElem = new Element(XMLHelper.ARGS_ELEM);
        elem.addContent(argsElem);
        // Arg+
        for(Integer argNr : argOrder) {
            // Arg
            Element argElem = new Element(XMLHelper.ARG_ELEM);
            argsElem.addContent(argElem);
            // attributes
            argElem.setAttribute(XMLHelper.NR_ATTR, Integer.toString(argNr));
        }
        
    }

    private static void handleAnswers(Element answersElement, List<String> answers) {
        for(String a : answers) {
            Element textElem = new Element(XMLHelper.TEXT_ELEM);
            answersElement.addContent(textElem);
            textElem.addContent(a);
        }
    }
    
}
