/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.fokus.carneades.questions;

import org.fokus.carneades.api.Statement;
import org.json.JSONException;
import org.json.JSONObject;

/**
 *
 * @author stb
 */
// TODO : implement some useful mapping using ontology annotations
public class QuestionHelper {
    
    public static Question getQuestionFromStatement(Statement stmt) {
        Question question = new Question();
        String q = stmt.toString();
        question.setQuestion(q);
        question.setHint(q);
        question.setCategory("default");
        return question;
    }
    
    public static JSONObject getJSONFromQuestion(Question q) throws JSONException{
        // TODO : what to do with id?
        // TODO : possible answers
        JSONObject jsonObj = new JSONObject("{ \"question\" : {\"id\":1, \"question\":'"+q.getQuestion()+": ', \"hint\":'"+q.getHint()+"', \"type\":'"+q.getType()+"'}}");
        return jsonObj;        
    }

}
