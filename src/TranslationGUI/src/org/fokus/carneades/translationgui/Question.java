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
public class Question {
    
    private int arg;
    private String type;
    private String hint;
    private String category;
    
    private Map<String, FormatText> formatTextMap;
    private List<String> answers;
    private List<QRef> refs;

    public Question(int arg, String type, String hint, String category, Map<String, FormatText> formatTextMap, List<String> answers, List<QRef> refs) {
        this.arg = arg;
        this.type = type;
        this.hint = hint;
        this.category = category;
        this.formatTextMap = formatTextMap;
        this.answers = answers;
        this.refs = refs;
    }

    public List<String> getAnswers() {
        return answers;
    }

    public void setAnswers(List<String> answers) {
        this.answers = answers;
    }

    public int getArg() {
        return arg;
    }

    public void setArg(int arg) {
        this.arg = arg;
    }

    public String getCategory() {
        return category;
    }

    public void setCategory(String category) {
        this.category = category;
    }

    public Map<String, FormatText> getFormatTextMap() {
        return formatTextMap;
    }

    public void setFormatTextMap(Map<String, FormatText> formatTextMap) {
        this.formatTextMap = formatTextMap;
    }

    public String getHint() {
        return hint;
    }

    public void setHint(String hint) {
        this.hint = hint;
    }

    public List<QRef> getRefs() {
        return refs;
    }

    public void setRefs(List<QRef> refs) {
        this.refs = refs;
    }

    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
    }

}
