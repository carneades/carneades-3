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
public class QRef {
    
    private String pred;
    private int arg;
    private List<Integer> argOrder;

    public QRef(String pred, int arg, List<Integer> argOrder) {
        this.pred = pred;
        this.arg = arg;
        this.argOrder = argOrder;
    }

    public int getArg() {
        return arg;
    }

    public void setArg(int arg) {
        this.arg = arg;
    }

    public List<Integer> getArgOrder() {
        return argOrder;
    }

    public void setArgOrder(List<Integer> argOrder) {
        this.argOrder = argOrder;
    }

    public String getPred() {
        return pred;
    }

    public void setPred(String pred) {
        this.pred = pred;
    }
        
}
