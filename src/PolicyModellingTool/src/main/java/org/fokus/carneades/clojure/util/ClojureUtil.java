/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.fokus.carneades.clojureutil;

import clojure.lang.RT;
import clojure.lang.Symbol;
import java.util.List;
import org.fokus.carneades.api.Statement;

/**
 * 
 * Utility class to convert between Clojure objects and the CarneadesAPI
 *
 * @author stb
 */
public class ClojureUtil {

    /**
     * 
     * Get CarneadesAPI statement from Clojure list
     * 
     * @param s clojure list
     * @return  CarneadesAPI statement representation of the clojure list
     */
    public static Statement getStatementFromSeq(List s) {
        Statement stmt = new Statement();
        stmt.setPredicate(s.get(0).toString());
        // System.out.println("stmt: "+stmt);
        for(Object o : s.subList(1, s.size())) {
            // System.out.println("o: "+o);
            stmt.getArgs().add(o.toString());
        }
        return stmt;

    }

    /**
     * 
     * Get Clojure list from CarneadesAPI statement
     * 
     * @param s CarneadesAPI statement
     * @return Clojure list representing the statement
     * @throws Exception 
     */
    public static List getSeqFromStatement(Statement s) throws Exception{
        Symbol pred = Symbol.intern(s.getPredicate());
        List seq = (List)RT.var("clojure.core","list").invoke(pred);
        for(String o : s.getArgs()) {            
            Object arg = RT.var(NS.CORE,"read-string").invoke(o);
            seq = (List)RT.var("clojure.core", "concat").invoke(seq, RT.var("clojure.core", "list").invoke(arg));
        }
        return seq;
    }

    /**
     * 
     * Get Clojure nested list from list of CarneadesAPI statement
     * 
     * @param l list of CarneadesAPI statement
     * @return Clojure nested list representing the statement list
     * @throws Exception 
     */
    public static List<List> getSeqFromStatementList(List<Statement> l) throws Exception{
        List<List> r = (List)RT.var("clojure.core","list").invoke();
        for(Statement s : l) {
            List stmtSeq = getSeqFromStatement(s);
            r = (List)RT.var("clojure.core", "concat").invoke(r, RT.var("clojure.core", "list").invoke(stmtSeq));
        }
        return r;
    }

}
