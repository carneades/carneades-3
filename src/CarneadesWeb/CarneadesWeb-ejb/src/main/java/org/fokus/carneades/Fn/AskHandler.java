/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.fokus.carneades.Fn;

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
 */
public class AskHandler {
    
    private class PredicateMap {
        
        public Map<String, List> map = new HashMap<String, List>();
        
    }    
    
    private Map<String,PredicateMap> answers = new HashMap<String,PredicateMap>();
    
    private static final Logger log = LoggerFactory.getLogger(AskHandler.class);
    
    public AskHandler(List<List> a) {        
        for(List s : a) {
            String predicate = RT.first(s).toString(); //s.get(0).toString();
            String subject = RT.second(s).toString(); // s.get(1).toString();
            log.info("predicate : "+predicate);
            log.info("subject : "+subject);
            log.info("statement: " + s.toString());
            PredicateMap predMap = this.answers.get(predicate);
            if(predMap == null) {
                predMap = new PredicateMap();
            }
            predMap.map.put(subject, s);
            this.answers.put(predicate, predMap);
        }
    }
    
    public Object getAnswer(List question, Map state) throws Exception{
        log.info("question: "+question.toString());
        log.info("getting predicate of goal");
        String pred = question.get(0).toString();
        log.info("checking if predicate has already been answered: " + pred);
        if (this.answers.containsKey(pred)) {
            // already answered
            PredicateMap predMap = this.answers.get(pred);
            log.info("getting subject of goal");
            String subj = RT.second(question).toString(); //question.get(1).toString();
            if(subj.startsWith("?")) {
                // TODO : arbitrary choice of answer here
                List answer = predMap.map.values().iterator().next();
                log.info("answer found: " + (String) RT.var(NS.CORE, "print-str").invoke(answer));
                log.info("replying to engine");
                Object o = RT.var(NS.ASK, "reply").invoke(state, question, answer);
                RT.var(NS.CORE, "println").invoke(o);
                return o;                
            } else {
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
