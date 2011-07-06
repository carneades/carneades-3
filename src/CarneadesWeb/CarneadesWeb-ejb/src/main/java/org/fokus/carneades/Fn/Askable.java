/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.fokus.carneades.Fn;

import clojure.lang.AFn;
import clojure.lang.RT;
import clojure.lang.Symbol;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 *
 * @author stb
 * 
 * a function used in carneades engine to check if a sub goal can be asked to the user
 * 
 */
public class Askable extends AFn{

    private static final Logger log = LoggerFactory.getLogger(Askable.class);
    private Map<Symbol,Boolean> predicates = new HashMap<Symbol,Boolean>();
    
    public Askable(List<String> preds) {
        for(String p : preds) {
            Symbol s = Symbol.intern(p);
            this.predicates.put(s,true);
        }
    }

    /**
     * 
     * @param arg1 sub goal that 
     * @return true if statement predicate is in teh predicates map; else false
     * @throws Exception 
     */
    @Override
    public Object invoke(Object arg1) throws Exception {
        Boolean ask = false;
        Symbol predicate = Symbol.intern("noValidArgument");
        if(arg1 instanceof List) {            
            predicate = (Symbol)RT.first(arg1);            
            ask = this.predicates.containsKey(predicate);
        }        
        log.info("askable? "+(String)RT.var("clojure.core","str").invoke(predicate)+" : "+ask.toString());
        return ask;
    }



}
