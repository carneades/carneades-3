package org.fokus.carneades.questions;

/**
 * List of questions will be saved here
 * @author bbr
 */
import java.util.HashMap;
import java.util.ArrayList;
public class Questions extends HashMap<String,ArrayList<Question>>
                       // { "questions" : [ { question } , ... ] }
{
        private String knowledgebase = "";

        public String getKnowledgebase(){
            return this.knowledgebase;
        }

        public void setKnowledgebase(String kb) {
            this.knowledgebase = kb;
        }

        public String getKB() {
            return this.getKnowledgebase();
        }

}