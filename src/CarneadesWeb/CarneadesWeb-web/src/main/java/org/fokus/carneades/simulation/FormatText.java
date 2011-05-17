/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.fokus.carneades.simulation;

import java.util.List;

/**
 *
 * @author stb
 */
public class FormatText {
    
    private String format;
    private List<Integer> args;

    public FormatText(String format, List<Integer> args) {
        this.format = format;
        this.args = args;
    }

    public List<Integer> getArgs() {
        return args;
    }

    public void setArgs(List<Integer> args) {
        this.args = args;
    }

    public String getFormat() {
        return format;
    }

    public void setFormat(String format) {
        this.format = format;
    }
    
    public String format(List<String> stmtArgs) {
        int l = this.args.size();
        String[] finalArgs = new String[l];
        for(Integer i :this.args) {
            finalArgs[i] = stmtArgs.get(this.args.get(i));
        }
        System.out.println("format   : "+this.format);
        System.out.println("finalArgs: "+finalArgs);
        return String.format(this.format, (Object[])finalArgs);
    }

}
