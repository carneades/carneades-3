/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.fokus.carneades;

import org.fokus.carneades.simulation.Translator;
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
import org.fokus.carneades.simulation.Answer;
import org.fokus.carneades.simulation.Question;
import org.fokus.carneades.simulation.QuestionHelper;
import org.json.JSONException;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 *
 * @author stb
 */
public class PolicySimulationServlet extends HttpServlet {

    private static final Logger log = LoggerFactory.getLogger(PolicySimulationServlet.class);
    private HttpSession session;
    // session content keys:
    private static final String CARNEADES_MANAGER = "CARNEADES_MANAGER";
    private static final String LAST_QUESTIONS = "LAST_QUESTIONS";
    private static final String LAST_SOLUTION = "LAST_SOLUTION";
    private static final String SOLUTION_PATH = "SOLUTION_PATH";
    private static final String LAST_OUT = "LAST_OUT";
    private static final String KNOWLEDGE_BASE = "KNOWLEDGE_BASE";
    private static final String QUERY = "QUERY";
    private static final String TRANSLATOR = "TRANSLATOR";
    private static final String LANGUAGE = "LANGUAGE";
    
    private static final String defaultLanguage = "en";

    /** 
     * Processes requests for both HTTP <code>GET</code> and <code>POST</code> methods.
     * @param request servlet request
     * @param response servlet response
     * @throws ServletException if a servlet-specific error occurs
     * @throws IOException if an I/O error occurs
     */
    protected void processRequest(HttpServletRequest request, HttpServletResponse response)
    throws ServletException, IOException {
        try {
            
            log.info("request in Policy Simulation Servlet");
            
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
            
            // getting translator
            // TODO : get translation file
            Translator translator = (Translator) session.getAttribute(TRANSLATOR);
            if(translator == null) {
                String translatorFile = "C:\\tmp\\translations.xml";
                translator = new Translator(translatorFile);
                session.setAttribute(TRANSLATOR, translator);
            } else {
                log.info("existing translator found");
            } 
            
            // getting language
            String language = (String)session.getAttribute(LANGUAGE);
            if(language == null) {
                language = defaultLanguage;
                session.setAttribute(LANGUAGE, defaultLanguage);
            } else {
                log.info("language found: "+language);
            }
            
            // incoming request
            String jsonINString = URLDecoder.decode(request.getParameter("json"),"UTF-8");
            JSONObject jsonIN = new JSONObject(jsonINString);
            
            log.info("jsonIN : "+jsonINString);
            
            // output
            // String jsonOUTString = "";
            JSONObject jsonOUT = new JSONObject();
            
            if (jsonIN.has("request")) {
                // initial request
                String topic = jsonIN.getString("request");
                if ("demo".equals(topic)) {
                    // return demo content
                    jsonOUT = handleDemoRequest();
                    //Questions questions = mapper.readValue(jsonOUT, Questions.class);
                    //saveQuestionsToSession(questions.getQuestions());
                }
                else {
                    log.info("getting args for topic:  " + topic);
                    // TODO : DB: topic -> statement, kb
                    // creating query for topic
                    log.info("creating query");
                    Statement query = new Statement();
                    query.setPredicate("timeForFatherAndGrandfather");
                    query.getArgs().add("Peter");
                    query.getArgs().add("?x");
                    query.getArgs().add("?y");
                    query.getArgs().add("?z");
                    // get kb for discussion
                    String kb = "http://localhost:8080/CarneadesWS-web/kb/lkif.xml";
                    //List<Question> qList = new ArrayList<Question>();
                    session.setAttribute(QUERY, query);
                    session.setAttribute(KNOWLEDGE_BASE, kb);
                    try {
                        jsonOUT = askEngine(service, query, kb, null, translator,language);
                    } catch (CarneadesException e) {
                        log.error(e.getMessage());
                        jsonOUT.put("error", e.getMessage());
                    }
                }
            }
            else if(jsonIN.has("answers")) {
                // client replies
                log.info("answers received from client: "+jsonINString);
                List<Question> qList = (ArrayList<Question>) session.getAttribute(LAST_QUESTIONS);
                String kb = (String) session.getAttribute(KNOWLEDGE_BASE);
                Statement query = (Statement) session.getAttribute(QUERY);
                List<Answer> answers = Answer.fromJSON(jsonIN);            
                List<Statement> answer = QuestionHelper.mapAnswersAndQuestionsToStatement(qList, answers);
                try {
                    jsonOUT =askEngine(service,query,kb,answer, translator,language);
                } catch (CarneadesException e) {
                    log.error(e.getMessage());
                    jsonOUT.put("error", e.getMessage());
                }
                
            } else if (jsonIN.has("language")) {
                String newLang = jsonIN.getString("language");
                session.setAttribute(LANGUAGE, newLang);
                MessageType lastMsg = (MessageType)session.getAttribute(LAST_OUT);
                if(MessageType.ASKUSER.equals(lastMsg)) {
                    List<Question> qList = (List<Question>)session.getAttribute(LAST_QUESTIONS);
                    jsonOUT = QuestionHelper.getJSONFromQuestions(qList, newLang);
                } else if (MessageType.SOLUTION.equals(lastMsg)) {
                    Statement sol = (Statement)session.getAttribute(LAST_SOLUTION);
                    String solPath = (String)session.getAttribute(SOLUTION_PATH);
                    jsonOUT.put("solution", translator.getStatementText(sol, newLang));
                    jsonOUT.put("path", solPath);
                } else {
                    jsonOUT = new JSONObject();
                    jsonOUT.put("language", newLang);
                }
                log.info("language updated: "+newLang);
            }
            else if (jsonIN == null || jsonINString.equals("null")) {
                // TODO : implement null case for jsonIN
                // null
                jsonOUT.put("error", "null can not be handled");
            }
            else {
                // no "json" request
                jsonOUT.put("error", "An error with your request has occurred");
            }

            // sending OUTPUT
            response.setContentType("text/html;charset=UTF-8");
            PrintWriter out = response.getWriter();
            try {
                out.println(jsonOUT.toString());                            
            } catch (Exception e) {
                log.error(e.getMessage());
                out.println("<pre>"+e.toString() +"</pre>");
            } finally {
                out.close();
            }
        } catch (JSONException ex) {
            ex.printStackTrace();
            log.error(ex.getMessage());
        }
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
    private JSONObject askEngine(CarneadesService service, Statement query, String kb, List<Statement> answer, Translator translator, String lang) throws CarneadesException, JSONException{
        List<Question> qList = new ArrayList<Question>();
        JSONObject jsonOUT = new JSONObject();
        // ask
        // TODO : get askables from CMS
        List<String> askables = new ArrayList<String>();
        askables.add("isFather");
        askables.add("hasAge");
        // askables.add("http://carneades/test/ont#s");
        log.info("calling ask from ejb: " + query.toString());
        CarneadesMessage msg = service.askEngine(query,kb,askables,answer);
        // evaluate answer
        if(msg != null) {
            log.info("call from ejb returned:"+msg.getType().toString());
        
            if (MessageType.ASKUSER.equals(msg.getType())) {
                // convert Statement to question
                //qList.addAll(CarneadesDatabase.getQuestionsFromStatement(msg.getMessage())); 
                Statement qStmt = msg.getMessage();
                List<Question> questionList = translator.getQuestions(qStmt);
                qList.addAll(questionList);
                // save questions in session
                session.setAttribute(LAST_QUESTIONS, qList);
                session.setAttribute(LAST_OUT, MessageType.ASKUSER);
                jsonOUT = QuestionHelper.getJSONFromQuestions(qList, lang);
            } else if (MessageType.SOLUTION.equals(msg.getType())) {
                // solution
                log.info("sending solution to user");
                // jsonOUT = "{\"solution\":"+msg.getAG()+"}";
                Statement solution = msg.getMessage();
                String solPath = msg.getAG();
                session.setAttribute(LAST_SOLUTION, solution);
                session.setAttribute(LAST_OUT, MessageType.SOLUTION);
                session.setAttribute(SOLUTION_PATH, solPath);
                jsonOUT.put("solution", translator.getStatementText(solution, lang));    
                jsonOUT.put("path", solPath);
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

    private JSONObject handleDemoRequest() throws JSONException {
        String r = "{\"questions\" : ["
                    + "{\"id\":1,\"type\":\"text\",\"question\":\"Forename: \",\"answers\":[\"\"],\"category\":\"Personal Information\", \"hint\":\"enter your full first name\"},"
                    + "{\"id\":2,\"type\":\"text\",\"question\":\"Last name: \",\"answers\":[\"\"],\"category\":\"Personal Information\", \"hint\":\"enter your full family name\"},"
                    + "{\"id\":3,\"type\":\"select\",\"question\":\"Country: \",\"answers\":[\"Austria\", \"Bulgarian (&#1073;&#1098;&#1083;&#1075;&#1072;&#1088;&#1089;&#1082;&#1080; &#1077;&#1079;&#1080;&#1082;)\",\"Germany (Deutschland)\",\"Polish (Polski)\"],\"category\":\"Personal Information\", \"hint\":\"where do you life\"},"
                    + "{\"id\":4,\"type\":\"radio\",\"question\":\"family status: \",\"answers\":[\"not married\",\"married\",\"divorced\"],\"category\":\"Family\", \"hint\":\"\"},"
                    + "{\"id\":5,\"type\":\"int\",\"question\":\"Number of children: \",\"answers\":[\"\"],\"category\":\"Family\", \"hint\":\"Please enter the number of children.\",\"optional\":true},"
                    + "{\"id\":6,\"type\":\"date\",\"question\":\"Birthday: \",\"answers\":[\"\"],\"category\":\"Family\", \"hint\":\"\"},"
                    + "{\"id\":7,\"type\":\"checkbox\",\"question\":\"Hobbies: \",\"answers\":[\"Paragliding\",\"boongie jumping\",\"sharkhunting\",\"jackass-like-stuns\",\"rocket science\"],\"category\":\"Personal Information\",\"hint:\":\"Please be honest.\"}"
                    + "]}";
        return new JSONObject(r);
    }

}
