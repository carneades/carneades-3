/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.fokus.carneades.translationgui;

import java.util.List;

/**
 *
 * @author stb
 */
public class FormatText {
    
    private String text;
    private List<Integer> argOder;

    public FormatText(String text, List<Integer> argOder) {
        this.text = text;
        this.argOder = argOder;
    }

    public List<Integer> getArgOder() {
        return argOder;
    }

    public void setArgOder(List<Integer> argOder) {
        this.argOder = argOder;
    }

    public String getText() {
        return text;
    }

    public void setText(String text) {
        this.text = text;
    }
    
}
