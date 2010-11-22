/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.fokus.carneades.api;

import clojure.lang.PersistentStructMap;

/**
 *
 * @author stb
 */
public class CarneadesMessage {

    private PersistentStructMap AG;
    private Statement message;
    private MessageType type;

       public CarneadesMessage() {
    }

    public Object getAG() {
        return AG;
    }

    public void setAG(PersistentStructMap State) {
        this.AG = State;
    }

    public Statement getMessage() {
        return message;
    }

    public void setMessage(Statement message) {
        this.message = message;
    }

     public MessageType getType() {
        return type;
    }

    public void setType(MessageType type) {
        this.type = type;
    }
    
}
