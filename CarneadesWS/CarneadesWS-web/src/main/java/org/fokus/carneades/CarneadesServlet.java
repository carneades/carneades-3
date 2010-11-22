/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.fokus.carneades;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.List;
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
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
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
        // getting answers
        String a = request.getParameter("answers");
        List<Statement> answers = handleRequestAnswers(a);

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
        } catch (Exception e) {
            out.println("<pre>"+e.toString() +"</pre>");
        } finally {
            out.close();
        }
    }
    
    private String askEngine(CarneadesService manager, List<Statement> answers) {
        String result = "";        
        // creating query        
        // TODO : get query for discussion
        log.info("creating query");
        Statement query = new Statement();
        query.setPredicate("p");
        query.getArgs().add("?x");
        // TODO : get kb for discussion
        String kb = "http://localhost:8080/CarneadesWS-web/kb/lkif.xml";
        // ask
        log.info("calling ask from ejb");
        CarneadesMessage msg = manager.askEngine(query, kb, answers);
        // evaluate answer
        log.info("call from ejb returned:"+msg.getType().toString());
        if (MessageType.ASKUSER.equals(msg.getType())) {
            // new question
            List<Question> qList = QuestionHelper.getQuestionsFromStatement(msg.getMessage());
            try {
                log.info("sending question to user");
                JSONObject question = QuestionHelper.getJSONFromQuestions(qList);
                result = question.toString();
            } catch (JSONException e) {
                log.error("could not create json object", e.toString());
                result = "Error: " + e.toString();
            }

        } else if (MessageType.SOLUTION.equals(msg.getType())) {
            // solution
            // TODO : implement solution case
            log.info("sending solution to user");
            result = "solution found";
        } else {
            // error
            log.error("unknown message type received from ask", msg);            
            result = "Error: unknown message type received from ask";
        }
        
        return result;
    }
    
    private List<Statement> handleRequestAnswers(String a) {
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
