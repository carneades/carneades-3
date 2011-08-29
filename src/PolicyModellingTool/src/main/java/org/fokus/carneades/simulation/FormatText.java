/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.fokus.carneades.simulation;

import java.util.List;

/**
 * 
 * Representation of a format string with an order on the arguments.
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
    
    /**
     * 
     * Formatting a string with the given arguments using the defined order.
     * 
     * ("%s foo bar %s", 1, 0) applied to "s0", "s1" will get:
     * "s1 foo bar s0"
     * 
     * @param stmtArgs list of arguments for the format string
     * @return  formatted string with args applied in the defined order.
     */
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
