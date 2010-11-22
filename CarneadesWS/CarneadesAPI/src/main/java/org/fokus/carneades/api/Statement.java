/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.fokus.carneades.api;

import java.util.ArrayList;
import java.util.List;

/**
 *
 * @author stb
 */
public class Statement {

    private String predicate;
    private List<String> args = new ArrayList<String>();

    public Statement() {
    }

    public List<String> getArgs() {
        return args;
    }

    public String getPredicate() {
        return predicate;
    }

    public void setPredicate(String predicate) {
        this.predicate = predicate;
    }

    @Override
    public String toString() {
        String r = "(" + this.predicate;
        for(String s : this.args) {
            r = r + " " + s;
        }
        r = r + ")";
        return r;
    }

}
