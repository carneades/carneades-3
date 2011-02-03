/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.fokus.carneades.simulation;

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
       
    public static String getJSONFromQuestions(List<Question> qList) {
        // TODO : what to do with id? - ID simply numbered, e.g. 1st question's id = 1, 2nd = 2 etc.
        // TODO : possible answers
        // TODO : multiple questions
        
        //OLD: JSONObject jsonObj = new JSONObject("{ \"questions\" : [{\"id\":1, \"question\":\""+q.getQuestion()+": \", \"hint\":\""+q.getHint()+"\", \"type\":\""+q.getType()+"\", \"category\" : \""+q.getCategory()+"\"}]}");

        // TODO : mapper not used
        ObjectMapper mapper = new ObjectMapper();
        String jsonObj = "{\"questions\":[";
        for (int i=0; i < qList.size(); i++) {
            Question q = qList.get(i);
            if (i > 0) jsonObj += ",";
            try {
                jsonObj += q.toString(); // mapper.writeValueAsString(q);
            }
            catch (Exception e) {
                // TODO : handle Exception
            }
        }
        jsonObj += "]}";

        return jsonObj;
    }

    public static List<Statement> mapAnswersAndQuestionsToStatement (List<Question> qList, List<Answer> aList) {
        // blah blah
        List<Statement> result = new ArrayList<Statement>();
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
                if (args.get(i).indexOf("?") == 0) {
                    // found asked argument
                    args.set(i, a.getValue());
                    break;
                }
            }
            stmt.setArgs(args);
            result.add(stmt);
        }
        return result;
    }

    public static List<Statement> mapAnswersAndQuestionsToStatement (Questions questions, Answers answers) {
        List<Question> qList = questions.getQuestions();
        List<Answer> aList = answers.getAnswers();
        return mapAnswersAndQuestionsToStatement(qList, aList);
    }

}
