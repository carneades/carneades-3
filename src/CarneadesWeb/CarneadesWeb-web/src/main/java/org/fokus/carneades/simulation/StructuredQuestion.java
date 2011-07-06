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
 * Structured representation of a question to be asked to the user.
 *
 * @author stb, bbr
 */

public class StructuredQuestion {
    
    private static final Logger log = LoggerFactory.getLogger(StructuredQuestion.class);
    
    // TODO : why id ?
    private int id; 
    
    // Translation grammar
    private int arg; // which argument of the statement is asked? (pred subj ?x) -> 1 ; (pred ?x) -> 0
    private String type; // string, int, date, ... 
    private Map<String, FormatText> formatMap; // map of language to format text
    private String hint; // hint text
    private String category; // category
    private List<String> possibleAnswers; // predefined answers, important for question type radio or list
    private List<QuestionRef> refs; // references to other question to be asked simultaniously
    
    private Statement statement; // question as carneades.api.Statement       
    private boolean optional; // flag for optional questions

    public StructuredQuestion(int arg, String type, Map<String, FormatText> formatMap, String hint, String category, List<String> possibleAnswers, List<QuestionRef> refs, boolean optional) {
        
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

    public StructuredQuestion(int arg, String type, Map<String, FormatText> formatMap, String hint, String category, List<String> possibleAnswers, List<QuestionRef> refs) {
        
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
    
    /**
     * 
     * Create JSON representation of the question object for given language.
     * 
     * @param lang language to be used
     * @return  json object representing this question object in the given language
     */
    public JSONObject toJSON(String lang) { 
        JSONObject jsonQuestion = new JSONObject();
        try {
            
            jsonQuestion.put("id", this.id);
            
            // format text in given language
            FormatText form = this.formatMap.get(lang);
            String formText = "undefined";            
            if(form == null) {
                // get format text in first language that is provided
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
            // type
            jsonQuestion.put("type", this.type);
            // possible answers
            if(!this.possibleAnswers.isEmpty()) {
                JSONArray jsonAnswers = new JSONArray();
                for(String s : this.possibleAnswers) {
                    jsonAnswers.put(s);
                }
                jsonQuestion.put("answers", jsonAnswers);
            }
            // hint
            jsonQuestion.put("hint", this.hint);
            // category
            jsonQuestion.put("category", this.category);
            // optional
            jsonQuestion.put("optional", this.optional);            
        } catch(JSONException e) {            
            log.error("could not transform question to json:" + e.getMessage(), this);
        } finally {
            return jsonQuestion;
        }        
    }

}
