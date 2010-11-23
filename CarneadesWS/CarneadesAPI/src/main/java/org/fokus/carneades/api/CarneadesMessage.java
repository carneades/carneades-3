/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.fokus.carneades.api;

import java.io.Serializable;
import java.util.Map;

/**
 *
 * @author stb
 */
public class CarneadesMessage implements Serializable{

    private Map AG;
    private Statement message;
    private MessageType type;

       public CarneadesMessage() {
    }

    public Object getAG() {
        return AG;
    }

    public void setAG(Map State) {
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
