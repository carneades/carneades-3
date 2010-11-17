/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.fokus.carneades.api;

/**
 *
 * @author stb
 */
public class CarneadesMessage {

    private Object State;
    private Statement message;

    public CarneadesMessage() {
    }

    public Object getState() {
        return State;
    }

    public void setState(Object State) {
        this.State = State;
    }

    public Statement getMessage() {
        return message;
    }

    public void setMessage(Statement message) {
        this.message = message;
    }

    
}
