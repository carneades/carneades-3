/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.fokus.carneades.Fn;

import clojure.lang.AFn;
import clojure.lang.PersistentStructMap;
import clojure.lang.RT;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 *
 * @author stb
 */
public class GetAnswer extends AFn{

    private static final Logger log = LoggerFactory.getLogger(GetAnswer.class);

    // TODO : we need a map of a map
    private Map<String,List> answers = new HashMap<String,List>();

    public GetAnswer(List<List> a) {
        for(List s : a) {
            this.answers.put(s.get(0).toString(), s);
        }
    }
    

    @Override
    public Object invoke(Object arg1, Object arg2) throws Exception {
        
        log.info("GetAnswer is invoked");

        if (arg1 instanceof List && arg2 instanceof PersistentStructMap) {
            log.info("GetAnswer has the right argument type");
            List goal = (List) arg1;            
            PersistentStructMap state = (PersistentStructMap)arg2;
            log.info("getting predicate of goal");
            String p = goal.get(0).toString();
            log.info("checking if predicate is already answered: "+p);
            if(this.answers.containsKey(p)) {
                // already answered
                log.info("predicate is already answered");
                log.info("replying to engine");
                Object o = RT.var("carneades.engine.ask","reply").invoke(state, goal, this.answers.get(p));
                return o;
            } else {
                log.info("predicate was not answered yet; asking user");
                throw new AskException(goal,state);
            }
        } else {
            log.info("GetAnswer was invoked with the wrong type for function call: "+arg1.getClass().getName()+" "+arg2.getClass().getName());
            throw new Exception("wrong argument type for function call");
        }

    }
    
    

}
