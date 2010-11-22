/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.fokus.carneades.questions;

import java.util.ArrayList;
import java.util.List;
import org.fokus.carneades.api.Statement;
import org.json.JSONException;
import org.json.JSONObject;

/**
 *
 * @author stb
 */
// TODO : implement some useful mapping using ontology annotations
public class QuestionHelper {
    
    public static List<Question> getQuestionsFromStatement(Statement stmt) {
        List<Question> result = new ArrayList<Question>();
        Question question = new Question();
        String q = stmt.toString();
        question.setQuestion(q);
        question.setHint(q);
        question.setType("text");
        question.setCategory("foo");
        result.add(question);
        return result;
    }
    
    public static JSONObject getJSONFromQuestions(List<Question> qList) throws JSONException{
        // TODO : what to do with id? - ID simply numbered, e.g. 1st question's id = 1, 2nd = 2 etc.
        // TODO : possible answers
        // TODO : multiple questions
        Question q = qList.get(0);
        JSONObject jsonObj = new JSONObject("{ \"questions\" : [{\"id\":1, \"question\":\""+q.getQuestion()+": \", \"hint\":\""+q.getHint()+"\", \"type\":\""+q.getType()+"\", \"category\" : \""+q.getCategory()+"\"}]}");
        return jsonObj;        
    }

}
