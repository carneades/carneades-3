/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.fokus.carneades.Fn;

import clojure.lang.PersistentStructMap;
import java.util.List;

/**
 *
 * @author stb
 */
public class AskException extends Exception{

    private List goal;
    private PersistentStructMap state;

    public AskException(List goal, PersistentStructMap state) {
        this.goal = goal;
        this.state = state;
    }

    public List getGoal() {
        return goal;
    }

    public PersistentStructMap getState() {
        return state;
    }

}
