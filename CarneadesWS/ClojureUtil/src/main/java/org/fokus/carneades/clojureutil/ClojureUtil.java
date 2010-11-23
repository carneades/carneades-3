/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.fokus.carneades.clojureutil;

import clojure.lang.RT;
import java.util.ArrayList;
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
        for(Object o : s.subList(1, s.size())) {
            stmt.getArgs().add(o.toString());
        }
        return stmt;

    }

    public static List getSeqFromStatement(Statement s) throws Exception{
        List seq = (List)RT.var("clojure.core", "list").invoke(s.getPredicate());
        for(String o : s.getArgs()) {
            seq.add(o);
        }
        return seq;
    }

    public static List<List> getSeqFromStatementList(List<Statement> l) throws Exception{
        List<List> r = new ArrayList<List>();
        for(Statement s : l) {
            r.add(getSeqFromStatement(s));
        }
        return r;
    }

}
