/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.fokus.carneades.Fn;

import clojure.lang.AFn;
import clojure.lang.LazySeq;
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
    private Map<String,LazySeq> answers = new HashMap<String,LazySeq>();

    public GetAnswer(List<LazySeq> a) {
        for(LazySeq s : a) {
            this.answers.put(s.first().toString(), s);
        }
    }
    

    @Override
    public Object invoke(Object arg1, Object arg2) throws Exception {

        if (arg1 instanceof LazySeq && arg2 instanceof PersistentStructMap) {
            LazySeq goal = (LazySeq) arg1;            
            PersistentStructMap state = (PersistentStructMap)arg2;
            String p = goal.first().toString();
            if(this.answers.containsKey(p)) {
                // already answered
                Object o = RT.var("carneades.engine.ask","reply").invoke(state, goal, this.answers.get(p));
                return o;
            } else {
                throw new AskException(goal,state);
            }
        } else {
            throw new Exception("wrong argument type for function call");
        }

    }
    
    

}
