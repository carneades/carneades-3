/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.fokus.carneades.questions;

import java.util.List;
import java.util.ArrayList;
import org.fokus.carneades.api.Statement;
import org.codehaus.jackson.annotate.JsonIgnore;

/**
 *
 * @author stb, bbr
 */
public class Question {
    
    
    private int id = 0;              // required, unique (at least for one request)
    private String question = "";    // required
    private String type = "text";    // required
    private List<String> answers = new ArrayList<String>(); // required
    private Statement statement;     // required
    private String hint = "";        // optional
    private String category ="";     // optional
    private boolean optional = false;// declares if the question is required or not


    public Question() {
        // TODO: ID generator?
    }

    @JsonIgnore
    public Question(int id) {
        this.id = id;
    }

    public int getId() {
        return id;
    }

    public void setId(int id) {
        this.id = id;
    }

    //@JsonIgnore
    public String getHint() {
        return hint;
    }

    //@JsonIgnore
    public void setHint(String hint) {
        this.hint = hint;
    }

    public String getQuestion() {
        return question;
    }

    public void setQuestion(String question) {
        this.question = question;
    }

    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
    }
    
    public String getCategory() {
        return category;
    }

    @JsonIgnore
    public void setCategory(String category) {
        this.category = category;
    }

    public List<String> getAnswers() {
        return answers;
    }

    public void setAnswers(List<String> answers) {
        this.answers = answers;
    }

    @JsonIgnore
    public void addAnswer(String answer) {
        this.answers.add(answer);
    }

    @JsonIgnore
    public Statement getStatement() {
        return statement;
    }

    @JsonIgnore
    public void setStatement(Statement statement) {
        this.statement = statement;
    }

    @JsonIgnore
    public boolean getOptional() {
        return optional;
    }

    @JsonIgnore
    public void setOptional(boolean optional) {
        this.optional = optional;
    }
    
    @Override
    public String toString() {
        String result = "{";
        result += "\"id\":"+getId()+"";
        result += ",\"question\":\""+getQuestion()+"\"";
        result += ",\"type\":\""+getType()+"\"";
        result += ",\"answers\":[";
        for (int i=0;i < answers.size(); i++) {
            result += "\""+answers.get(i)+"\"";
            if (i+1 < answers.size()) result += ",";
        }
        result += "]";
        if (!hint.isEmpty()) result += ",\"hint\":\""+getHint()+"\"";
        if (!category.isEmpty()) result += ",\"category\":\""+getCategory()+"\"";
        if (optional) result += ",\"optional\":"+getOptional()+"";
        result += "}";
        return result;
    }

}
