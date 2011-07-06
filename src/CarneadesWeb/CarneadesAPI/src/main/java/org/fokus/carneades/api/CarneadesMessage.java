/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.fokus.carneades.api;

import java.io.Serializable;
import java.util.List;

/**
 *
 * @author stb
 * 
 * a class bundling several responses from the carneades service
 * 
 */
public class CarneadesMessage implements Serializable{

    private String AG;
    private Statement message;
    private List<Statement> statements;
    private MessageType type;

    public CarneadesMessage() {
        this.AG = null;
        this.message = null;
        this.type = MessageType.SOLUTION;
        this.statements = null;
    }

    public List<Statement> getStatements() {
        return statements;
    }

    public void setStatements(List<Statement> statements) {
        this.statements = statements;
    }


    public String getAG() {
        return AG;
    }

    public void setAG(String State) {
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
