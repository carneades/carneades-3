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
    private Integer[] args;

    public FormatText(String format, Integer[] args) {
        this.format = format;
        this.args = args;
    }

    public Object[] getArgs() {
        return args;
    }

    public void setArgs(Integer[] args) {
        this.args = args;
    }

    public String getFormat() {
        return format;
    }

    public void setFormat(String format) {
        this.format = format;
    }
    
    public String format(List<String> stmtArgs) {
        int l = this.args.length;
        String[] finalArgs = new String[l];
        for(int i=0; i<l; i++) {
            finalArgs[i] = stmtArgs.get(this.args[i]);
        }
        return String.format(this.format, (Object)finalArgs);
    }

}
