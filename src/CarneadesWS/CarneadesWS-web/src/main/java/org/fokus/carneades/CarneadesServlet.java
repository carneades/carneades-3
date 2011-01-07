/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.fokus.carneades;

import java.io.IOException;
import java.io.PrintWriter;
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
import org.fokus.carneades.questions.*;
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
        ObjectMapper mapper = new ObjectMapper();
        if ( jsonIN.matches("\\s*{\\s*\"request\"\\s*:.+") ) {
            // initial request
            if ( jsonIN.matches("\\s*{\\s*\"request\"\\s*:\\s*\"demo\"\\s*}\\s*") ) {
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
            else if(jsonIN.matches("\\s*{\\s*\"request\"\\s*:\\s*\"\\w\"\\s*}\\s*")) {
                // finding requested topic
                String topic = jsonIN.replaceFirst("\\s*{\\s*\"request\"\\s*:\\s*\"","");
                topic = topic.replaceFirst("\"\\s*}\\s*","");
                // getting the topic
                Map<Statement, String> statement_value;
                // TODO : DB access to access statementlist and knowledgebase
                    // getting the topic's statements
                    Statement statement1 = new Statement();
                    statement1.setPredicate("p");
                    statement1.getArgs().add("?x");
                    statement_value.put(statement1, null); // null cuz there is no answer given yet
                    // get kb for discussion
                    String kb = "http://localhost:8080/CarneadesWS-web/kb/lkif.xml";
                ArrayList<Question> qList;
                try {
                    // engine really needed here?
                    qList = askEngine(service,statement_value,kb);
                    jsonOUT = QuestionHelper.getJSONFromQuestions(qList);
                } catch (CarneadesException e) {
                    log.error(e.getMessage());
                    jsonOUT = e.getMessage();
                }
                // save questions in session
                if (qList.size() > 0) {
                    Questions qListSave = new Questions();
                    qListSave.put("questions", qList);
                    qListSave.setKB(kb);
                    session.setAttribute("LatestQuestions", qListSave);
                    jsonOUT = QuestionHelper.getJSONFromQuestions(qList);
                    log.info("sending question to user");
                }
            }
        }
        else if(jsonIN.matches("\\s*{\\s*\"answers\"\\s*:.+")) {
            // client replies
            Map<Statement, String> statement_value = null;
            // put saved statement (stored in question) and given answer in a map
            Questions questions = (Questions) session.getAttribute("LatestQuestions");
            String kb = questions.getKB();
            ArrayList<Question> qList = questions.get(((ArrayList)questions.keySet()).get(0));
            Answers answers = mapper.readValue(jsonIN, Answers.class);
            ArrayList<Answer> aList = answers.get(((ArrayList)answers.keySet()).get(0));
            for (int i=0; i<aList.size();i++) {
                int id = aList.get(i).getId();
                for (int j=0; j<qList.size();j++) {
                    if (id == qList.get(j).getId()) {
                        // found matching question
                        statement_value.put(qList.get(j).getStatement(),
                                            aList.get(i).getAnswer());
                        break;
                    }
                }
            }
            ArrayList<Question> qListNew = new ArrayList();
            try {
                qListNew = askEngine(service,statement_value,kb);
            } catch (CarneadesException e) {
                log.error(e.getMessage());
                jsonOUT = e.getMessage();
            }
            // save questions in session
            if (qListNew.size() > 0) {
                Questions qListNewSave = new Questions();
                qListNewSave.put("questions", qListNew);
                qListNewSave.setKB(kb);
                session.setAttribute("LatestQuestions", qListNewSave);
                jsonOUT = QuestionHelper.getJSONFromQuestions(qListNew);
                log.info("sending question to user");
            }
            else jsonOUT = "Solution found.";
        }
        else if (jsonIN == null || jsonIN.equals("null")) {
            // null
            jsonOUT = "null can not be handled";
        }
        else {
            // no "json" request
            jsonOUT = "An error with your request has occurred.";
        }

        // OUTPUT
        response.setContentType("text/html;charset=UTF-8");
        PrintWriter out = response.getWriter();
        try {
            out.println(jsonOUT);
        /*} catch (NumberFormatException e) {
            out.println("<pre>"+e.toString() +"</pre>");
        } catch (UndeclaredThrowableException e){
            log.error(e.getUndeclaredThrowable().getMessage());
            out.println("<pre>"+e.toString());
            e.getCause().printStackTrace(out);
            out.println("</pre>");
            e.printStackTrace();*/
        } catch (Exception e) {
            log.error(e.getMessage());
            out.println("<pre>"+e.toString() +"</pre>");
        } finally {
            out.close();
        }
    }
    
    private ArrayList<Question> askEngine(CarneadesService service, Map<Statement, String> statement_value, String kb) throws CarneadesException {
        ArrayList<Question> qList = new ArrayList();
        ArrayList<Statement> sList = (ArrayList) statement_value.keySet();
        // creating query        
        log.info("creating query");
        for (int i=0; sList.size() < i; i++) {
            Statement query = sList.get(i);
            String answer = statement_value.get(query); // can be String / int / float / etc.
            // ask
            log.info("calling ask from ejb: " + query.toString());
            CarneadesMessage msg = service.askEngine(query, kb, answer);
            // evaluate answer
            log.info("call from ejb returned:"+msg.getType().toString());
            if (MessageType.ASKUSER.equals(msg.getType())) {
                // new question
                qList.addAll(QuestionHelper.getQuestionsFromStatement(msg.getMessage()));
            } else if (MessageType.SOLUTION.equals(msg.getType())) {
                // solution
                // TODO : implement solution case
                log.info("sending solution to user");
                // SolutionToAJAX()
            } else {
                // error
                log.error("unknown message type received from ask: {}", msg);
                throw new CarneadesException("Unknown message type received from ask: "+msg);
            }
        }
        return qList;
    }
    
    /*private List<Statement> handleRequestAnswers(String a) { // is this function still needed?
        //Questions qList = mapper.readValue(a, Questions.class);
        Answers rootAsMap = mapper.readValue(a, Answers.class);
        List<Answer> = rootAsMap.get("answers");
        List<Statement> result = new List<Statement>();
        // TODO : implement answer handling
        return result;
    }*/

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
