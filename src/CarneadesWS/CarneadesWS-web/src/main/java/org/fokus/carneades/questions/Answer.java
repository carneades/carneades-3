/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.fokus.carneades.questions;

/**
 *
 * @author bbr
 */
public class Answer {
    
    
    private int id = 0;             // required, unique (at least for one request)
    private String value = "";     // required
    // TODO: String as type? maybe change the type arcording to type in Question.


    public Answer() {
        // TODO: ID generator?
    }

    public Answer(int id) {
        this.id = id;
    }

    public int getId() {
        return id;
    }

    public void setId(int id) {
        this.id = id;
    }

    public String getValue() {
        return value;
    }

    public void setValue(String answer) {
        this.value = answer;
    }

}