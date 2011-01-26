/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.fokus.carneades;

import java.util.List;
import java.util.ArrayList;
import org.fokus.carneades.api.Statement;
import org.fokus.carneades.questions.Question;
import org.fokus.carneades.questions.Questions;
/**
 *
 * @author bbr
 */
public class CarneadesDatabase {

    /**
     * conects to the database to get the question(s) matching a statement
     * @param stmt the question's statement
     * @return a complete question
     */
    public static List<Question> getQuestionsFromStatement(Statement stmt) {
        // TODO : DB ACCESS! (what DB? CMS connection?)
        // statement to multiple statemens: name --> forename, last name or city -> city, zip, street
        // in normal case only one question per statement

        // creating a demo-only question because no DB yet
        List<Question> questions = new ArrayList<Question>();
        //Questions result = new Questions();

        Question question = new Question();
        String q = stmt.toString();
        question.setQuestion(q);
        question.setId(1);
        question.setType("text");
        question.setStatement(stmt);
        question.setCategory("foo");
        question.setHint(q);
        question.addAnswer("");

        questions.add(question);
        //result.setQuestions(questions);
        //return result;

        return questions;
    }

}
