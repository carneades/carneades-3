/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.fokus.carneades.translationgui;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 *
 * @author stb
 */
public class TranslationHelper {
    
    public static List<String> getLanguagesForStmt(Predicate predicate) {
        
        List<String> languages = new ArrayList<String>();
        Set<String> languageSet = new HashSet<String>();        
        
        Map<String, FormatText> formatMap = predicate.getFormatTextMap();
        languageSet = formatMap.keySet();
        
            
        Iterator<String> langIter = languageSet.iterator();
        while(langIter.hasNext()) {
            languages.add(langIter.next());
        }
        
        return languages;
        
    }
    
    public static List<String> getLanguagesForQuestion(Question question) {
        
        List<String> languages = new ArrayList<String>();
        
        if(question != null) {
            Iterator<String> iter = question.getFormatTextMap().keySet().iterator();
            while(iter.hasNext()) {
                languages.add(iter.next());
            }
        }
        
        return languages;
        
    }
    
    public static Question getQuestionForArg(List<Question> questions, int arg) {
        
        for(Question q : questions) {
            if(q.getArg() == arg) {
                return q;
            }
        }
        
        return null;
    }
    
}
