/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.fokus.carneades;

import java.io.IOException;
import java.io.PrintWriter;
import java.lang.reflect.UndeclaredThrowableException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;
import org.fokus.carneades.api.CarneadesMessage;
import org.fokus.carneades.api.MessageType;
import org.fokus.carneades.api.Statement;
import org.fokus.carneades.common.EjbLocator;
import org.fokus.carneades.questions.Question;
import org.fokus.carneades.questions.QuestionHelper;
import org.codehaus.jackson.map.ObjectMapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 *
 * @author stb
 */
public class CarneadesServlet extends HttpServlet {

    private static final Logger log = LoggerFactory.getLogger(CarneadesServlet.class);
    private static final String CARNEADES_MANAGER = "CARNEADES_MANAGER";

    /** 
     * Processes requests for both HTTP <code>GET</code> and <code>POST</code> methods.
     * @param request servlet request
     * @param response servlet response
     * @throws ServletException if a servlet-specific error occurs
     * @throws IOException if an I/O error occurs
     */
    protected void processRequest(HttpServletRequest request, HttpServletResponse response)
    throws ServletException, IOException {
        log.info("request in Carneses Servlet");
        // INPUT
        HttpSession session = request.getSession();
        // getting Session Bean
        log.info("getting stateful session bean");
        CarneadesService service = (CarneadesService) session.getAttribute(CARNEADES_MANAGER);
        if (service == null) {
            log.info("creating new session bean");
            service = EjbLocator.getCarneadesService();
        } else {
            log.info("existing bean found");
        }
        // incoming request
        String jsonIN = request.getParameter("json");
        String jsonOUT = "";
        // DO WHAT?
        if ( jsonIN.matches("\s*{\s*\"request\"\s*:.+") ) {
            // initial request
            if ( jsonIN.matches("\s*{\s*\"request\"\s*:\s*\"demo\"\s*}\s*") ) {
                // give back demo content
                jsonOUT = "{\"questions\" : ["
                + "{\"id\":1,\"type\":\"text\",\"question\":\"Forename: \",\"answers\":[\"\"],\"category\":\"Personal Information\", \"hint\":\"enter your full first name\"},"
                + "{\"id\":2,\"type\":\"text\",\"question\":\"Last name: \",\"answers\":[\"\"],\"category\":\"Personal Information\", \"hint\":\"enter your full family name\"},"
                + "{\"id\":3,\"type\":\"select\",\"question\":\"Country: \",\"answers\":[\"Austria\", \"Bulgarian (&#1073;&#1098;&#1083;&#1075;&#1072;&#1088;&#1089;&#1082;&#1080; &#1077;&#1079;&#1080;&#1082;)\",\"Germany (Deutschland)\",\"Polish (Polski)\"],\"category\":\"Personal Information\", \"hint\":\"where do you life\"},"
                + "{\"id\":4,\"type\":\"radio\",\"question\":\"family status: \",\"answers\":[\"not married\",\"married\",\"divorced\"],\"category\":\"Family\", \"hint\":\"\"},"
                + "{\"id\":5,\"type\":\"int\",\"question\":\"Number of children: \",\"answers\":[\"\"],\"category\":\"Family\", \"hint\":\"Please enter the number of children.\",\"optional\":true},"
                + "{\"id\":6,\"type\":\"date\",\"question\":\"Birthday: \",\"answers\":[\"\"],\"category\":\"Family\"},"
                + "{\"id\":7,\"type\":\"checkbox\",\"question\":\"Hobbies: \",\"answers\":[\"Paragliding\",\"boongie jumping\",\"sharkhunting\",\"jackass-like-stuns\",\"rocket science\"],\"category\":\"Personal Information\",\"hint:\":\"Please be honest.\"}"
                + "]}";
            }
            else if(jsonIN.matches("\s*{\s*\"request\"\s*:\s*\"\w\"\s*}\s*")) {
                // finding requested topic
                String topic = jsonIN.replaceFirst("\s*{\s*\"request\"\s*:\s*\"","");
                topic = topic.replaceFirst("\"\s*}\s*","");
                // getting the topic
                Map<Statement, Object> statement_value;
                // TODO : DB access to access statementlist and knowledgebase
                    // TODO : getting the topic's statements
                    Statement statement1 = new Statement();
                    statement1.setPredicate("p");
                    statement1.getArgs().add("?x");
                    statement_value.put(statement1, null); // null cuz there is no answer given yet
                    // TODO : get kb for discussion
                    String kb = "http://localhost:8080/CarneadesWS-web/kb/lkif.xml";
                // TODO : generate Questions and save them in the session
                jsonOUT = askEngine(service,statement_value,kb); // engine needed here?
            }
        }
        else if(jsonIN.matches("\s*{\s*\"answers\"\s*:.+")) {
            // reply
            Map<Statement, Object> statement_value;
            // TODO : put saved statement (stored in question) and given answer in a map
            Questions questions = session.getAttribute("LatestQuestions");
            List<Question> qList = questions.get(questions.keySet()[0]);
            Answers answers = mapper.readValue(jsonIN, Answers.class);
            List<Answer> aList = answers.get(answers.keySet()[0]);
            for (int i=0; i<aList.size();i++) {
                int id = aList.get(i).getId();
                for (int i=0; i<qList.size();i++) {
                    if (id == qList.get(i).getId()) {
                        // found matching question
                        statement_value.put(qList.get(i).getStatement(),
                                            aList.get(i).getAnswer());
                        break;
                    }
                }
            }

        }
        else if (jsonIN == null || jsonIN.equals("null")) {
            // null
        }

        List<Statement> answers = handleRequestAnswers(jsonIN);

        // OUTPUT
        response.setContentType("text/html;charset=UTF-8");
        PrintWriter out = response.getWriter();
        String outMsg = "";
        try {
            // if no answers given with the request               
                
                // TODO : getting answers
                outMsg = askEngine(service,answers);

                out.println(outMsg);

            
            // Questions interpretation
            // TODO : read last answer here
            /*
            else if (answer.indexOf("\",")) {
                String[] answers = answer.split("/\"\\s*,\\s*\"/"); // split at ","
                answers[0]=answers[0].substring(2);
                if (answers.length > 1) answers[answers.length-1]=answers[0].substring(0,answers[0].length()-3);
                // DB Zugriff hier
                try {
                    JSONArray answers2 = new JSONArray(answers);
                    out.println(answers2.toString());
                } catch (JSONException ee) {
                    log.error("could not create json object: ",ee.toString());
                    out.println("Error: "+ee.toString());
                }
            }*/
        } catch (NumberFormatException e) {
            out.println("<pre>"+e.toString() +"</pre>");
        } catch (UndeclaredThrowableException e){
            log.error(e.getUndeclaredThrowable().getMessage());
            out.println("<pre>"+e.toString());
            e.getCause().printStackTrace(out);
            out.println("</pre>");
            e.printStackTrace();
        } catch (Exception e) {
            log.error(e.getMessage());
            out.println("<pre>"+e.toString() +"</pre>");
        } finally {
            out.close();
        }
    }
    
    private String askEngine(CarneadesService service, Map<Statement, Object> statement_value, String kb) {
        String result = "";
        List<Question> qList;
        List<Statement> sList = statement_value.keySet();
        // creating query        
        log.info("creating query");
        for (int i=0; sList.size() < i; i++) {
            Statement query = sList[i];
            Object answer = statement_value.get(query); // can be String / int / float / etc.
            // ask
            log.info("calling ask from ejb: " + query.toString());
            CarneadesMessage msg = service.askEngine(query, kb, answer);
            // evaluate answer
            log.info("call from ejb returned:"+msg.getType().toString());
            if (MessageType.ASKUSER.equals(msg.getType())) {
                // new question
                qList.add(QuestionHelper.getQuestionsFromStatement(msg.getMessage()));
                log.info("sending question to user");
                result = QuestionHelper.getJSONFromQuestions(qList);
            } else if (MessageType.SOLUTION.equals(msg.getType())) {
                // solution
                // TODO : implement solution case
                log.info("sending solution to user");
                result = "solution found";
            } else {
                // error
                log.error("unknown message type received from ask: {}", msg);
                result = "Error: unknown message type received from ask";
            }
        }
        return result;
    }
    
    private List<Statement> handleRequestAnswers(String a) {
        //Questions qList = mapper.readValue(a, Questions.class);
        Answers rootAsMap = mapper.readValue(a, Answers.class);
        ArrayList<Answer> = rootAsMap.get("answers");
        List<Statement> result = new ArrayList<Statement>();
        // TODO : implement answer handling
        return result;
    }

    // <editor-fold defaultstate="collapsed" desc="HttpServlet methods. Click on the + sign on the left to edit the code.">
    /** 
     * Handles the HTTP <code>GET</code> method.
     * @param request servlet request
     * @param response servlet response
     * @throws ServletException if a servlet-specific error occurs
     * @throws IOException if an I/O error occurs
     */
    @Override
    protected void doGet(HttpServletRequest request, HttpServletResponse response)
    throws ServletException, IOException {
        processRequest(request, response);
    } 

    /** 
     * Handles the HTTP <code>POST</code> method.
     * @param request servlet request
     * @param response servlet response
     * @throws ServletException if a servlet-specific error occurs
     * @throws IOException if an I/O error occurs
     */
    @Override
    protected void doPost(HttpServletRequest request, HttpServletResponse response)
    throws ServletException, IOException {
        processRequest(request, response);
    }

    /** 
     * Returns a short description of the servlet.
     * @return a String containing servlet description
     */
    @Override
    public String getServletInfo() {
        return "Short description";
    }// </editor-fold>

}
