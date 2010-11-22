/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.fokus.carneades.clojureutil;

import clojure.lang.LazySeq;
import clojure.lang.RT;
import java.util.ArrayList;
import java.util.List;
import org.fokus.carneades.api.Statement;

/**
 *
 * @author stb
 */
public class ClojureUtil {

    public static Statement getStatementFromSeq(LazySeq s) {
        Statement stmt = new Statement();
        stmt.setPredicate(s.first().toString());
        for(Object o : s) {
            stmt.getArgs().add(o.toString());
        }
        return stmt;

    }

    public static LazySeq getSeqFromStatement(Statement s) throws Exception{
        LazySeq seq = (LazySeq)RT.var("clojure.core", "list").invoke(s.getPredicate());
        for(String o : s.getArgs()) {
            seq.add(o);
        }
        return seq;
    }

    public static List<LazySeq> getSeqFromStatementList(List<Statement> l) throws Exception{
        List<LazySeq> r = new ArrayList<LazySeq>();
        for(Statement s : l) {
            r.add(getSeqFromStatement(s));
        }
        return r;
    }

}
