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
        // statement to multiple statemens: name --> forename, last name
        // in normal case only one question per statement
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
                jsonObj += mapper.writeValueAsString(q); // or just q.toString()
            }
            catch (Exception e) {
            }
        }
        jsonObj += "]}";

        return jsonObj;
    }

    public static List<Statement> mapAnswersAndQuestionsToStatement (List<Question> qList, List<Answer> aList) {
        // blah blah
        List<Statement> result = new ArrayList();
        for (Answer a : aList) {
            int id = a.getId();
            Question q = null;
            for (Question q1 : qList) {
                if (id == q1.getId()) q = q1;
            }
            Statement stmt = q.getStatement();
            //for (String arg : stmt.getArgs()) {
            List<String> args = stmt.getArgs();
            for (int i=0; i < args.size(); i++) {
                String arg = args.get(i);
                if (arg.indexOf("?") == 0) {
                    // found asked argument
                    arg = a.getAnswer();
                    break;
                }
            }
            stmt.setArgs(args);
            result.add(stmt);
        }
        return result;
    }

    public static List<Statement> mapAnswersAndQuestionsToStatement (Questions questions, Answers answers) {
        ArrayList<Question> qList = questions.get(((ArrayList)questions.keySet()).get(0));
        ArrayList<Answer> aList = answers.get(((ArrayList)answers.keySet()).get(0));
        return mapAnswersAndQuestionsToStatement(qList, aList);
    }

}
