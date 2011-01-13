package org.fokus.carneades.questions;

/**
 * List of questions will be saved here
 * @author bbr
 */
import java.util.HashMap;
import java.util.ArrayList;
import org.fokus.carneades.api.Statement;

public class Questions extends HashMap<String,ArrayList<Question>>
                       // { "questions" : [ { question } , ... ] }
{
        private String knowledgebase = "";
        private Statement query;

        public Questions(){
            // do nothing
        }
        
        public Questions(String kb) {
            this.knowledgebase = kb;
        }

        public String getKnowledgebase(){
            return this.knowledgebase;
        }

        public void setKnowledgebase(String kb) {
            this.knowledgebase = kb;
        }

        public String getKB() {
            return this.getKnowledgebase();
        }

        public void setKB(String kb) {
            this.setKnowledgebase(kb);
        }

        public Statement getQuery(){
            return this.query;
        }

        public void setQuery(Statement q) {
            this.query = q;
        }


}