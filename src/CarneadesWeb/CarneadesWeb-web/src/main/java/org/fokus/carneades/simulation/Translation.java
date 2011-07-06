/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.fokus.carneades.simulation;

import java.util.List;
import java.util.Map;

/**
 * 
 * The Translation bundles all relevant information about a predicate:
 *  - number of arguments
 *  - a question object for each argument
 *  - a format string for the statement
 *
 * @author stb
 */
public class Translation {
    
    private String predicate;
    private int args;
    //  Statement stmt;
    private Map<Integer, StructuredQuestion> questions;
    //private Map<Question, List<QuestionRef>> questionRefs;
    private Map<String, FormatText> text;

    public Translation(String predicate, int args, Map<Integer, StructuredQuestion> questions, Map<String, FormatText> text) {
        this.predicate = predicate;
        this.args = args;
        this.questions = questions;
        //this.questionRefs = questionRefs;
        this.text = text;
    }
    
    public int getArgs() {
        return args;
    }

    public void setArgs(int args) {
        this.args = args;
    }

    public String getPredicate() {
        return predicate;
    }

    public void setPredicate(String predicate) {
        this.predicate = predicate;
    }

    public Map<Integer, StructuredQuestion> getQuestions() {
        return questions;
    }

    public void setQuestions(Map<Integer, StructuredQuestion> questions) {
        this.questions = questions;
    }

    public Map<String, FormatText> getText() {
        return text;
    }

    public void setText(Map<String, FormatText> text) {
        this.text = text;
    }
        
}
