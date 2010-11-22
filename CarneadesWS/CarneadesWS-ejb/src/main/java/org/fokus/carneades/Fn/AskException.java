/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.fokus.carneades.Fn;

import clojure.lang.LazySeq;
import clojure.lang.PersistentStructMap;

/**
 *
 * @author stb
 */
public class AskException extends Exception{

    private LazySeq goal;
    private PersistentStructMap state;

    public AskException(LazySeq goal, PersistentStructMap state) {
        this.goal = goal;
        this.state = state;
    }

    public LazySeq getGoal() {
        return goal;
    }

    public PersistentStructMap getState() {
        return state;
    }

}
