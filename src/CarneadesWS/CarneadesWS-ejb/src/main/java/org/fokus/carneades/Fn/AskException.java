/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.fokus.carneades.Fn;

import clojure.lang.AFn;
import java.util.List;
import java.util.Map;

/**
 *
 * @author stb
 */
public class AskException extends Exception{

    private List goal;
    private Map state;
    private AFn returnFn;

    public AskException(List goal, Map state, AFn returnFn) {
        this.goal = goal;
        this.state = state;
        this.returnFn = returnFn;
    }

    public List getGoal() {
        return goal;
    }

    public Map getState() {
        return state;
    }

    public AFn getReturnFn() {
        return returnFn;
    }
        

}
