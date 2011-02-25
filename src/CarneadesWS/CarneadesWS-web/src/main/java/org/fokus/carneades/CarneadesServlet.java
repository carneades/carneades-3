/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.fokus.carneades;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.List;
import java.net.URLDecoder;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;
import org.fokus.carneades.api.CarneadesMessage;
import org.fokus.carneades.api.MessageType;
import org.fokus.carneades.api.Statement;
import org.fokus.carneades.common.EjbLocator;
import org.codehaus.jackson.map.ObjectMapper;
import org.fokus.carneades.simulation.Answers;
import org.fokus.carneades.simulation.Answer;
import org.fokus.carneades.simulation.Question;
import org.fokus.carneades.simulation.QuestionHelper;
import org.fokus.carneades.simulation.Questions;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 *
 * @author stb
 */
public class CarneadesServlet extends HttpServlet {

    private static final Logger log = LoggerFactory.getLogger(CarneadesServlet.class);
    private HttpSession session;
    // session content keys:
    private static final String CARNEADES_MANAGER = "CARNEADES_MANAGER";
    private static final String LATEST_QUESTIONS = "LATEST_QUESTIONS";
    private static final String KNOWLEDGE_BASE = "KNOWLEDGE_BASE";
    private static final String QUERY = "QUERY";
    

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
        session = request.getSession();
        // getting Session Bean
        log.info("getting stateful session bean");
        CarneadesService service = (CarneadesService) session.getAttribute(CARNEADES_MANAGER);
        if (service == null) {
            log.info("creating new session bean");
            service = EjbLocator.getCarneadesService();
            session.setAttribute(CARNEADES_MANAGER, service);
        } else {
            log.info("existing bean found");
        }
        // incoming request
        String jsonIN = URLDecoder.decode(request.getParameter("json"),"UTF-8");
        //log.info(jsonIN);
        String jsonOUT = "";
        // DO WHAT?
        ObjectMapper mapper = new ObjectMapper();
        if ( jsonIN.matches("\\s*\\{\\s*\"request\"\\s*:.+") ) {
            // initial request
            if ( jsonIN.matches("\\s*\\{\\s*\"request\"\\s*:\\s*\"demo\"\\s*\\}\\s*") ) {
                // return demo content
                jsonOUT = "{\"questions\" : ["
                + "{\"id\":1,\"type\":\"text\",\"question\":\"Forename: \",\"answers\":[\"\"],\"category\":\"Personal Information\", \"hint\":\"enter your full first name\"},"
                + "{\"id\":2,\"type\":\"text\",\"question\":\"Last name: \",\"answers\":[\"\"],\"category\":\"Personal Information\", \"hint\":\"enter your full family name\"},"
                + "{\"id\":3,\"type\":\"select\",\"question\":\"Country: \",\"answers\":[\"Austria\", \"Bulgarian (&#1073;&#1098;&#1083;&#1075;&#1072;&#1088;&#1089;&#1082;&#1080; &#1077;&#1079;&#1080;&#1082;)\",\"Germany (Deutschland)\",\"Polish (Polski)\"],\"category\":\"Personal Information\", \"hint\":\"where do you life\"},"
                + "{\"id\":4,\"type\":\"radio\",\"question\":\"family status: \",\"answers\":[\"not married\",\"married\",\"divorced\"],\"category\":\"Family\", \"hint\":\"\"},"
                + "{\"id\":5,\"type\":\"int\",\"question\":\"Number of children: \",\"answers\":[\"\"],\"category\":\"Family\", \"hint\":\"Please enter the number of children.\",\"optional\":true},"
                + "{\"id\":6,\"type\":\"date\",\"question\":\"Birthday: \",\"answers\":[\"\"],\"category\":\"Family\", \"hint\":\"\"},"
                + "{\"id\":7,\"type\":\"checkbox\",\"question\":\"Hobbies: \",\"answers\":[\"Paragliding\",\"boongie jumping\",\"sharkhunting\",\"jackass-like-stuns\",\"rocket science\"],\"category\":\"Personal Information\",\"hint:\":\"Please be honest.\"}"
                + "]}";
                //Questions questions = mapper.readValue(jsonOUT, Questions.class);
                //saveQuestionsToSession(questions.getQuestions());
            }
            else if(jsonIN.matches("\\s*\\{\\s*\"request\"\\s*:\\s*\"\\w+\"\\s*\\}\\s*")) {
                // finding requested topic
                String topic = jsonIN.replaceFirst("\\s*\\{\\s*\"request\"\\s*:\\s*\"", "");
                topic = topic.replaceFirst("\"\\s*\\}\\s*", "");
                // getting the topic
                log.info("getting args for topic:  " + topic);
                // TODO : CMS: query & kb for topic
                // creating query for topic
                log.info("creating query");
                Statement query = new Statement();
                query.setPredicate("p");
                query.getArgs().add("?x");
                session.setAttribute(QUERY, query);
                // get kb for discussion
                String kb = "http://localhost:8080/CarneadesWS-web/kb/lkif.xml";
                session.setAttribute(KNOWLEDGE_BASE, kb);
                try {
                    jsonOUT = askEngine(service, query, kb, null); // empty = no questions yet
                } catch (CarneadesException e) {
                    log.error(e.getMessage());
                    jsonOUT = e.getMessage();
                }
            } else
                jsonOUT = "Your request doesn't match.";
        }
        else if(jsonIN.matches("\\s*\\{\\s*\"answers\"\\s*:.+")) {
            // client replies
            List<Question> qList = (ArrayList<Question>) session.getAttribute(LATEST_QUESTIONS);
            String kb = (String) session.getAttribute(KNOWLEDGE_BASE);
            Statement query = (Statement) session.getAttribute(QUERY);
            Answers answers = mapper.readValue(jsonIN, Answers.class);
            List<Answer> aList = answers.getAnswers();
            List<Statement> answer = QuestionHelper.mapAnswersAndQuestionsToStatement(qList, aList);
            try {
                jsonOUT =askEngine(service,query,kb,answer);
            } catch (CarneadesException e) {
                log.error(e.getMessage());
                jsonOUT = e.getMessage();
            }
            
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

    /**
     * saves the current questions, kb and query in the user session
     * @param q questions including knowledgebase and query
     */
    private void saveQuestionsToSession(List<Question> qList) {
        session.setAttribute(LATEST_QUESTIONS, qList);
        log.info("user session updated");
    }

    /**
     * saves the current questions, kb and query in the user session
     * @param q questions including knowledgebase and query
     */
    private void saveQuestionsToSession(Questions q) {
        session.setAttribute(LATEST_QUESTIONS, q.getQuestions());
        log.info("user session updated");
    }

    /**
     * Sends requests to the carneades-engine
     * @param service carneades service instance
     * @param query overall goal of the request (solution that has to be found)
     * @param kb location of the knowlegebase (rulebase) in the file system
     * @param answer actual list of questions including or not including the given answers by the user in the statement form
     * @return returns json representing either questions or solution
     * @throws CarneadesException if the request was not successful
     */
    private String askEngine(CarneadesService service, Statement query, String kb, List<Statement> answer) throws CarneadesException {
        List<Question> qList = new ArrayList<Question>();
        String jsonOUT = "";
        // ask
        // TODO : get askables from CMS
        List<String> askables = new ArrayList<String>();
        askables.add("http://carneades/test/ont#r");
        askables.add("http://carneades/test/ont#s");
        log.info("calling ask from ejb: " + query.toString());
        CarneadesMessage msg = service.askEngine(query,kb,askables,answer);
        // evaluate answer
        if(msg != null) {
            log.info("call from ejb returned:"+msg.getType().toString());
        
            if (MessageType.ASKUSER.equals(msg.getType())) {
                // convert Statement to question
                qList.addAll(CarneadesDatabase.getQuestionsFromStatement(msg.getMessage()));
                // save questions in session
                saveQuestionsToSession(qList);
                jsonOUT = QuestionHelper.getJSONFromQuestions(qList);
            } else if (MessageType.SOLUTION.equals(msg.getType())) {
                // solution
                log.info("sending solution to user");
                jsonOUT = "{\"solution\":\""+msg.getAG().replaceAll("\"", "'") +"\"}";
                // SolutionToAJAX()
            } else {
                // error
                log.error("unknown message type received from ask: {}", msg.getMessage().toString());
                throw new CarneadesException("Unknown message type received from ask: " + msg.getMessage().toString());
            }
        } else {
            log.info("call from ejb returned: null");
            throw new CarneadesException("engine returned null.");
        }
        return jsonOUT;
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
