/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.fokus.carneades.simulation;

import java.util.ArrayList;
import java.util.List;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 *
 * @author bbr
 */
public class Answer {
    
    private static final Logger log = LoggerFactory.getLogger(Answer.class);
    
    private int id;             // required, unique (at least for one request)
    private String value;     // required
    // TODO: String as type? maybe change the type arcording to type in Question.

    public static List<Answer> fromJSON(JSONObject obj) {
        List<Answer> answers = new ArrayList<Answer>();
        try {
            JSONArray answersArray = obj.getJSONArray("answers");
            for(int i=0; i<answersArray.length(); i++) {
                JSONObject jsonAnswer = (JSONObject)answersArray.get(i);
                int id = jsonAnswer.getInt("id");
                String answer = jsonAnswer.getString("value");
                answers.add(new Answer(id, answer));
            }
        } catch (JSONException ex) {
            ex.printStackTrace();
            log.error("could not transform from json: "+obj.toString());
        }
        return answers;
    }

    public Answer(int id, String value) {
        this.id = id;
        this.value = value;
    }

    public int getId() {
        return id;
    }

    public void setId(int id) {
        this.id = id;
    }

    public String getValue() {
        return value;
    }

    public void setValue(String answer) {
        this.value = answer;
    }

}