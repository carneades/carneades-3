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
 * @author stb
 */
public class ClojureUtil {

    public static Statement getStatementFromSeq(List s) {
        Statement stmt = new Statement();
        stmt.setPredicate(s.get(0).toString());
        System.out.println("stmt: "+stmt);
        for(Object o : s.subList(1, s.size())) {
            System.out.println("o: "+o);
            stmt.getArgs().add(o.toString());
        }
        return stmt;

    }

    public static List getSeqFromStatement(Statement s) throws Exception{
        Symbol pred = Symbol.intern(s.getPredicate());
        List seq = (List)RT.var("clojure.core","list").invoke(pred);
        for(String o : s.getArgs()) {            
            Object arg = RT.var(NS.CORE,"read-string").invoke(o);
            seq = (List)RT.var("clojure.core", "concat").invoke(seq, RT.var("clojure.core", "list").invoke(arg));
        }
        return seq;
    }

    public static List<List> getSeqFromStatementList(List<Statement> l) throws Exception{
        List<List> r = (List)RT.var("clojure.core","list").invoke();
        for(Statement s : l) {
            List stmtSeq = getSeqFromStatement(s);
            r = (List)RT.var("clojure.core", "concat").invoke(r, RT.var("clojure.core", "list").invoke(stmtSeq));
        }
        return r;
    }

}
