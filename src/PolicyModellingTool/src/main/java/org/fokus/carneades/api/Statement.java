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
 * 
 * simple java representation of lkif statements having one predicate and several arguments (subject, objects)
 * 
 * in most cases (predicate subject object) 
 * 
 */
public class Statement implements Serializable{
    
    // TODO : maybe we need nested statements?

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
