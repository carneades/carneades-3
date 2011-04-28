/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.fokus.carneades.translationgui;

import java.util.List;
import java.util.Map;

/**
 *
 * @author stb
 */
public class Predicate {
    
    private String pred;
    private int args;
    
    private Map<String, FormatText> formatTextMap;
    private List<Question> questions;

    public Predicate(String pred, int args, Map<String, FormatText> stmt, List<Question> questions) {
        this.pred = pred;
        this.args = args;
        this.formatTextMap = stmt;
        this.questions = questions;
    }

    public int getArgs() {
        return args;
    }

    public void setArgs(int args) {
        this.args = args;
    }

    public String getPred() {
        return pred;
    }

    public void setPred(String pred) {
        this.pred = pred;
    }

    public List<Question> getQuestions() {
        return questions;
    }

    public void setQuestions(List<Question> questions) {
        this.questions = questions;
    }

    public Map<String, FormatText> getFormatTextMap() {
        return formatTextMap;
    }

    public void setFormatTextMap(Map<String, FormatText> stmt) {
        this.formatTextMap = stmt;
    }   
    
    
}
