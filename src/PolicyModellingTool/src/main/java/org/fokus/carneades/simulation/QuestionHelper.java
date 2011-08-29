/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.fokus.carneades.simulation;

import java.util.ArrayList;
import java.util.List;
import org.fokus.carneades.api.Statement;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


/**
 * 
 * Utility class to handle some operations on questions.
 *
 * @author stb, bbr
 */
// TODO : implement some useful mapping using ontology annotations

public class QuestionHelper {
    
    private static final Logger log = LoggerFactory.getLogger(QuestionHelper.class);
       
    /**
     * 
     * Get JSON representation of a list of question
     * 
     * @param qList List of question objects.
     * @param lang Language to be used
     * @return json object representing the list of questions
     */
    public static JSONObject getJSONFromQuestions(List<StructuredQuestion> qList, String lang) {
        // TODO : what to do with id? - ID simply numbered, e.g. 1st question's id = 1, 2nd = 2 etc.

        JSONObject jsonQuestions = new JSONObject();
        try {            
            JSONArray qArray = new JSONArray();
            for(StructuredQuestion q : qList) {
                qArray.put(q.toJSON(lang));
            }
            jsonQuestions.put("questions", qArray);
        } catch (JSONException e) {
            log.error("could not transform questions: " + e.getMessage(), qList);
        } finally {
            return jsonQuestions;
        }
    }

    /**
     * 
     * Map answers from the web client to the questions.
     * 
     * @param qList List of questions that have been asked to the web client
     * @param aList List of answers received from the web client
     * @return  List of statements where answers and questions have been matched
     */
    public static List<Statement> mapAnswersAndQuestionsToStatement (List<StructuredQuestion> qList, List<Answer> aList) {
        List<Statement> result = new ArrayList<Statement>();
        for (Answer answer : aList) {
            
            // find corresponding question
            StructuredQuestion q = findQuestion(answer.getId(), qList);
            // find index for questioned object
            Statement stmt = q.getStatement();                        
            int index = findQuestionPos(stmt);
            // replace object with answer
            stmt.getArgs().set(index, answer.getValue());
            
            result.add(stmt);
        }
        for(Statement s : result) {
            log.info(s.toString());
        }
        return result;
    }
    
    private static int findQuestionPos(Statement q) {
        List<String> args = q.getArgs();
        for(int i=0; i<args.size(); i++) {
            if(args.get(i).startsWith("?")) {
                return i;
            }
        }
        return (args.size()-1);
    }

    private static StructuredQuestion findQuestion(int id, List<StructuredQuestion> qList) {
        for(StructuredQuestion q : qList) {
            if(q.getId() == id) {
                return q;
            }
        }
        return null;
    }

}
