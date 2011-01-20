package org.fokus.carneades.questions;

/**
 * List of questions will be saved here
 * @author bbr
 */
import java.util.HashMap;
import java.util.List;
import java.util.ArrayList;
import org.fokus.carneades.api.Statement;
import org.codehaus.jackson.annotate.JsonIgnore;

public class Questions {
    // { "questions" : [ { question } , ... ] }
    private List<Question> questions = new ArrayList();
    private String knowledgebase = "";
    private Statement query;

    public Questions(){
        // do nothing
    }

    @JsonIgnore
    public Questions(String kb) {
        this.knowledgebase = kb;
    }

    @JsonIgnore
    public String getKnowledgebase(){
        return this.knowledgebase;
    }

    @JsonIgnore
    public void setKnowledgebase(String kb) {
        this.knowledgebase = kb;
    }

    @JsonIgnore
    public String getKB() {
        return this.getKnowledgebase();
    }

    @JsonIgnore
    public void setKB(String kb) {
        this.setKnowledgebase(kb);
    }

    @JsonIgnore
    public Statement getQuery(){
        return this.query;
    }

    @JsonIgnore
    public void setQuery(Statement q) {
        this.query = q;
    }

    /**
     * @return the questions
     */
    public List<Question> getQuestions() {
        return questions;
    }

    /**
     * @param questions the questions to set
     */
    public void setQuestions(List<Question> questions) {
        this.questions = questions;
    }

    /**
     * 
     * @return Returns all Questions in JSON format
     */
    @JsonIgnore
    @Override
    public String toString() {
        String result = "{\"questions\":[";
        for (int i=0;i < questions.size(); i++) {
            result += questions.get(i).toString();
            if (i+1 < questions.size()) result += ",";
        }
        result += "]}";
        return result;
    }

    /**
     * @param q adds a list of question to questions
     */
    @JsonIgnore
    public void addAll(Questions q) {
        this.questions.addAll(q.getQuestions());
        if (this.knowledgebase.isEmpty()) this.knowledgebase = q.getKnowledgebase();
        if (this.query.getArgs().size() == 0) this.query = q.getQuery();
    }

}