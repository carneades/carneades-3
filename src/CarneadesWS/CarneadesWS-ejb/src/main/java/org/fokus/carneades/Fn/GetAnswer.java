/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.fokus.carneades.Fn;

import clojure.lang.AFn;
import clojure.lang.Keyword;
import clojure.lang.RT;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.fokus.carneades.NS;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 *
 * @author stb
 */
public class GetAnswer extends AFn{
    
    private class PredicateMap {
        
        public Map<String, List> map = new HashMap<String, List>();
        
    }
    

    private static final Logger log = LoggerFactory.getLogger(GetAnswer.class);

    // TODO : we need a map of a map
    private Map<String,PredicateMap> answers = new HashMap<String,PredicateMap>();
    private AFn continuation = null;

    public GetAnswer(List<List> a, AFn ret) {
        this.continuation = ret;
        for(List s : a) {
            String predicate = s.get(0).toString();
            String subject = s.get(1).toString();
            PredicateMap predMap = this.answers.get(predicate);
            if(predMap == null) {
                predMap = new PredicateMap();
            }
            predMap.map.put(subject, s);
            this.answers.put(predicate, predMap);
        }
    }
        
    private Object getAnswer(List goal, Map state) throws Exception {
        log.info("getting predicate of goal");
        String pred = goal.get(0).toString();
        log.info("checking if predicate has already been answered: " + pred);
        if (this.answers.containsKey(pred)) {
            // already answered
            PredicateMap predMap = this.answers.get(pred);
            log.info("getting subject of goal");
            String subj = goal.get(1).toString();
            log.info("checking if subject has already been answered: " + subj);
            if (predMap.map.containsKey(subj)) {
                List answer = predMap.map.get(subj);
                log.info("answer found: " + (String) RT.var(NS.CORE, "print-str").invoke(answer));
                log.info("replying to engine");
                Object o = RT.var(NS.ASK, "reply").invoke(state, goal, answer);
                RT.var(NS.CORE, "println").invoke(o);
                return o;
            } else {
                log.info("subject was not answered yet; asking user");
                throw new AskException(goal, state, this.continuation);
            }
        } else {
            log.info("predicate was not answered yet; asking user");
            throw new AskException(goal, state, this.continuation);
        }
    }
    

    @Override
    public Object invoke(Object arg1, Object arg2) throws Exception {
        
        log.info("GetAnswer is invoked");

        if (arg1 instanceof List && arg2 instanceof Map) {
            log.info("GetAnswer has the right argument type");            
            Map state = (Map)arg2;  
            Object subs = state.get(Keyword.intern("substitutions"));
            RT.var(NS.CORE,"println").invoke(state);
            RT.var(NS.CORE,"println").invoke(subs);
            List goal = (List) RT.var(NS.UNIFY, "apply-substitution").invoke(subs,arg1);  
            return getAnswer(goal, state);
            
        } else {
            log.info("GetAnswer was invoked with the wrong type for function call: "+arg1.getClass().getName()+" "+arg2.getClass().getName());
            throw new Exception("wrong argument type for function call");
        }

    }
    
    @Override
    public Object invoke(Object arg1) throws Exception {
        
        if(arg1 instanceof AFn) {
            log.info("continuation cached");
            AFn cont = (AFn) arg1;
            this.continuation = cont;
            return cont.invoke(this);
        } else {
            log.error("GetAnswer argument has wrong type: "+arg1.getClass().toString(), arg1);
            return null;
        }
                
    }

}
