/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.fokus.carneades.simulation;

import java.util.List;
import java.util.ArrayList;
import org.fokus.carneades.api.Statement;
import org.codehaus.jackson.annotate.JsonIgnore;

/**
 *
 * @author stb, bbr
 */
// TODO : check all the @JsonIgnore annotations
public class Question {
    
    // TODO : why id ?
    private int id = 0;              // required, unique (at least for one request)
    private FormatText question = null;    // required
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

    public FormatText getQuestion() {
        return question;
    }

    public void setQuestion(FormatText question) {
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
    
    // TODO : revise Question.toString() method
    public String toJSON() {
        String result = "{";
        result += "\"id\":"+this.id+"";
        result += ",\"question\":\""+this.question.format(this.statement.getArgs()) +"\"";
        result += ",\"type\":\""+this.type+"\"";
        if(this.answers.size() > 0) {
            result += ",\"answers\":[";
            for (int i=0;i < this.answers.size(); i++) {
                result += "\""+this.answers.get(i)+"\"";
                if (i+1 < this.answers.size()) result += ",";
            }
            result += "]"; 
        }
        if (!this.hint.isEmpty()) result += ",\"hint\":\""+this.hint+"\"";
        if (!this.category.isEmpty()) result += ",\"category\":\""+this.category+"\"";
        if (this.optional) result += ",\"optional\":"+this.optional+"";
        result += "}";
        return result;
    }

}
