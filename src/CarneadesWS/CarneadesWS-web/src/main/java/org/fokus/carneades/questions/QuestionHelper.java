/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.fokus.carneades.questions;

import java.util.ArrayList;
import java.util.List;
import org.fokus.carneades.api.Statement;
import org.codehaus.jackson.map.ObjectMapper;


/**
 *
 * @author stb, bbr
 */
// TODO : implement some useful mapping using ontology annotations
public class QuestionHelper {
    
    public static List<Question> getQuestionsFromStatement(Statement stmt) {
        // TODO : generate multiple questions out of one statement (DB?)
        List<Question> result = new ArrayList<Question>();
        Question question = new Question();
        String q = stmt.toString();
        question.setQuestion(q);
        question.setType("text");
        question.setStatement(stmt);
        question.setCategory("foo");
        question.setHint(q);
        result.add(question);
        return result;
    }
    
    public static String getJSONFromQuestions(List<Question> qList) {
        // TODO : what to do with id? - ID simply numbered, e.g. 1st question's id = 1, 2nd = 2 etc.
        // TODO : possible answers
        // TODO : multiple questions
        
        //OLD: JSONObject jsonObj = new JSONObject("{ \"questions\" : [{\"id\":1, \"question\":\""+q.getQuestion()+": \", \"hint\":\""+q.getHint()+"\", \"type\":\""+q.getType()+"\", \"category\" : \""+q.getCategory()+"\"}]}");

        ObjectMapper mapper = new ObjectMapper();
        String jsonObj = "{\"questions\":[";
        for (int i=0; i < qList.size(); i++) {
            Question q = qList.get(i);
            if (i > 0) jsonObj += ",";
            try {
                jsonObj += mapper.writeValueAsString(q);
            }
            catch (Exception e) {
            }
        }
        jsonObj += "]}";

        return jsonObj;
    }

}
