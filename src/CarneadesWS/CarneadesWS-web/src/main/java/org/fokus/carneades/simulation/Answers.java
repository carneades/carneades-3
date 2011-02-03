package org.fokus.carneades.simulation;

/**
 * List of answers will be saved here
 * @author bbr
 */

import java.util.List;
import java.util.ArrayList;
import org.codehaus.jackson.annotate.JsonIgnore;

public class Answers {
                       // { "answers" : [ { answer } , ... ] }

    private List<Answer> answers = new ArrayList();

    /**
     * @return the answers
     */
    public List<Answer> getAnswers() {
        return answers;
    }

    /**
     * @param answers the answers to set
     */
    public void setAnswers(List<Answer> answers) {
        this.answers = answers;
    }

    /**
     * @param q adds a list of answers to answers
     */
    @JsonIgnore
    public void addAll(Answers q) {
        this.answers.addAll(q.getAnswers());
    }

}