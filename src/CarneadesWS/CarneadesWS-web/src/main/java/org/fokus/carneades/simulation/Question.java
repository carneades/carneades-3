/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.fokus.carneades.simulation;

import java.util.List;
import java.util.Map;
import org.fokus.carneades.api.Statement;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 *
 * @author stb, bbr
 */

public class Question {
    
    private static final Logger log = LoggerFactory.getLogger(Question.class);
    
    // TODO : why id ?
    private int id; 
    
    // Translation grammar
    private int arg;
    private String type;    
    private Map<String, FormatText> formatMap;    
    private String hint;
    private String category;
    private List<String> possibleAnswers;
    private List<QuestionRef> refs;
    
    private Statement statement;            
    private boolean optional;

    public Question(int arg, String type, Map<String, FormatText> formatMap, String hint, String category, List<String> possibleAnswers, List<QuestionRef> refs, boolean optional) {
        
        this.id = 0;
        
        this.arg = arg;
        this.type = type;
        this.formatMap = formatMap;
        this.hint = hint;
        this.category = category;
        this.possibleAnswers = possibleAnswers;
        this.refs = refs;
        
        this.optional = optional;        
        this.statement = null;
    }

    public Question(int arg, String type, Map<String, FormatText> formatMap, String hint, String category, List<String> possibleAnswers, List<QuestionRef> refs) {
        
        this.id = 0;
        
        this.arg = arg;
        this.type = type;
        this.formatMap = formatMap;
        this.hint = hint;
        this.category = category;
        this.possibleAnswers = possibleAnswers;
        this.refs = refs;
        
        this.statement = null;
        this.optional = false;
    }

    public List<String> getPossibleAnswers() {
        return possibleAnswers;
    }

    public void setPossibleAnswers(List<String> answers) {
        this.possibleAnswers = answers;
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

    public Map<String, FormatText> getFormatMap() {
        return formatMap;
    }

    public void setFormatMap(Map<String, FormatText> formatMap) {
        this.formatMap = formatMap;
    }

    public String getHint() {
        return hint;
    }

    public void setHint(String hint) {
        this.hint = hint;
    }

    public int getId() {
        return id;
    }

    public void setId(int id) {
        this.id = id;
    }

    public boolean isOptional() {
        return optional;
    }

    public void setOptional(boolean optional) {
        this.optional = optional;
    }

    public List<QuestionRef> getRefs() {
        return refs;
    }

    public void setRefs(List<QuestionRef> refs) {
        this.refs = refs;
    }

    public Statement getStatement() {
        return statement;
    }

    public void setStatement(Statement statement) {
        this.statement = statement;
    }

    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
    }
    
    public JSONObject toJSON(String lang) { 
        JSONObject jsonQuestion = new JSONObject();
        try {
            
            jsonQuestion.put("id", this.id);
            FormatText form = this.formatMap.get(lang);
            String formText = "undefined";
            if(form == null) {
                // get first language                
                String fallBackLang = this.formatMap.keySet().iterator().next();
                form = this.formatMap.get(fallBackLang);
                log.warn("could not find language for question: "+lang);
                log.warn("using language: "+fallBackLang);
                formText = GoogleTranslate.translate(form.format(this.statement.getArgs()), fallBackLang, lang);
            } else {
                formText = form.format(this.statement.getArgs());
            }
            log.info("question to json: "+ formText);
            log.info("question to json: "+ this.statement.getArgs());
            jsonQuestion.put("question", formText);
            jsonQuestion.put("type", this.type);
            if(!this.possibleAnswers.isEmpty()) {
                JSONArray jsonAnswers = new JSONArray();
                for(String s : this.possibleAnswers) {
                    jsonAnswers.put(s);
                }
                jsonQuestion.put("answers", jsonAnswers);
            }
            jsonQuestion.put("hint", this.hint);
            jsonQuestion.put("category", this.category);
            jsonQuestion.put("optional", this.optional);            
        } catch(JSONException e) {            
            log.error("could not transform question to json:" + e.getMessage(), this);
        } finally {
            return jsonQuestion;
        }        
    }

}
