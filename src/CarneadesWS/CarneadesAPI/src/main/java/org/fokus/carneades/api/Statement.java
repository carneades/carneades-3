/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.fokus.carneades.api;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

/**
 *
 * @author stb
 */
public class Statement implements Serializable{

    private String predicate;
    private List<String> args;

    public Statement() {
        predicate = "";
        args = new ArrayList<String>();
    }

    public List<String> getArgs() {
        return args;
    }

    public void setArgs(List<String> args) {
        this.args = args;
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
