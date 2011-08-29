/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.fokus.carneades.engine.fn;

import clojure.lang.RT;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.fokus.carneades.clojureutil.NS;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 *
 * @author stb
 * 
 * The AskHandler stores already given user answers and constructs replys to questions that have been already answered
 * 
 * Answers are stored in a map of maps:
 * 
 * predicate -> (subject -> statement)
 * 
 */
public class AskHandler {
    
    /**
     * subject -> statement     *  
     */    
    private class SubjectMap {
        
        public Map<String, List> map = new HashMap<String, List>();
        
    }    
    
    /**
     * predicate -> subjectMap
     */
    private Map<String,SubjectMap> answers = new HashMap<String,SubjectMap>();
    
    private static final Logger log = LoggerFactory.getLogger(AskHandler.class);
    
    /**
     * constructor
     * gets a list of answers and stores them in the map
     * 
     * @param a list of clojure statements
     */
    public AskHandler(List<List> a) {        
        for(List s : a) {
            String predicate = RT.first(s).toString(); //s.get(0).toString();
            String subject = RT.second(s).toString(); // s.get(1).toString();
            log.info("predicate : "+predicate);
            log.info("subject : "+subject);
            log.info("statement: " + s.toString());
            SubjectMap predMap = this.answers.get(predicate);
            if(predMap == null) {
                predMap = new SubjectMap();
            }
            predMap.map.put(subject, s);
            this.answers.put(predicate, predMap);
        }
    }
    
    /**
     * 
     * look if question was already answered; throw exception if not
     * 
     * @param question clojure statement being the sub goal in the argumentation process
     * @param state current search state
     * @return reply if already ansered
     * @throws Exception if question wasn't answered yet
     */
    public Object getAnswer(List question, Map state) throws Exception{
        log.info("question: "+question.toString());
        log.info("getting predicate of goal");
        String pred = question.get(0).toString();
        log.info("checking if predicate has already been answered: " + pred);
        if (this.answers.containsKey(pred)) {
            // already answered
            SubjectMap predMap = this.answers.get(pred);
            log.info("getting subject of goal");
            String subj = RT.second(question).toString(); //question.get(1).toString();            
            if(subj.startsWith("?")) {
                // question = (pred ?x ...)
                // TODO : arbitrary choice of answer here
                List answer = predMap.map.values().iterator().next();
                log.info("answer found: " + (String) RT.var(NS.CORE, "print-str").invoke(answer));
                log.info("replying to engine");
                Object o = RT.var(NS.ASK, "reply").invoke(state, question, answer);
                RT.var(NS.CORE, "println").invoke(o);
                return o;                
            } else {
                // question = (pred subj ...)
                log.info("checking if subject has already been answered: " + subj);            
                if (predMap.map.containsKey(subj)) {
                    List answer = predMap.map.get(subj);
                    log.info("answer found: " + (String) RT.var(NS.CORE, "print-str").invoke(answer));
                    log.info("replying to engine");
                    Object o = RT.var(NS.ASK, "reply").invoke(state, question, answer);
                    RT.var(NS.CORE, "println").invoke(o);
                    return o;
                } else {
                    log.info("subject was not answered yet; asking user");
                    throw new AskException();
                }
            }
        } else {
            log.info("predicate was not answered yet; asking user");
            throw new AskException();
        }
    }

}
