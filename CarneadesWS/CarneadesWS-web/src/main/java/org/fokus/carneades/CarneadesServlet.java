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
        // INPUT
        HttpSession session = request.getSession();
        // TODO : what is test for?
        String test = request.getParameter("test");
        String requestType = request.getParameter("reqType");

        // OUTPUT
        response.setContentType("text/html;charset=UTF-8");
        PrintWriter out = response.getWriter();
        String outMsg = "";
        try {
            // request for topics 
            // TODO : what is this for?
            if ("topics".equals(requestType)) {
                int topic_ID = Integer.parseInt(request.getParameter("id"));
                outMsg = getTopics();                
            }
            // request for carneades engine
            else if ("askEngine".equals(requestType)) {
                // getting Session Bean
                CarneadesServiceManager manager = (CarneadesServiceManager) session.getAttribute(CARNEADES_MANAGER);
                if (manager == null) {
                    manager = EjbLocator.getCarneadesService();
                }
                // TODO : getting answers
                outMsg = askEngine(manager);
            }
            // Fragen holen
            // TODO : what is this for?
            else if("questions".equals(requestType)) {
                int category = Integer.parseInt(request.getParameter("id"));
                outMsg = questions(category);
            }
            
            out.println(outMsg);
            
            // Fragen abliefern
            // TODO : what happens here?
            if ("topics".equals(requestType)) {
                String[] answers = request.getParameter("answers").split("/\"\\s*,\\s*\"/"); // split bei ","
                answers[0]=answers[0].substring(2);
                if (answers.length > 1) answers[answers.length-1]=answers[0].substring(0,answers[0].length()-3);
                // DB Zugriff hier
                try {
                    JSONArray answers2 = new JSONArray(answers);
                    out.println(answers2.toString());
                } catch (JSONException ee) {
                    log.error("could not create json object",ee.toString());
                    out.println("Error: "+ee.toString());
                }
            }
            /* default */
            // does this combination of json and html work?
            if (requestType == null || requestType.equals("")) {
                out.println("<html>");
                out.println("<head>");
                out.println("<title>Servlet carnserv</title>");
                out.println("</head>");
                out.println("<body>");
                out.println("<h1>Pfad: " + request.getContextPath () + "</h1>");
                out.println("<p>test</p>");
                // TODO : ???
                if (test != null && (test == null ? "" != null : !test.equals(""))) {
                    out.println("Ausgewählt: <pre>"+test+"</pre>");
                }
                out.println("<form action=\"/carn-dummy-serv/carnserv\" method=\"POST\">");
                out.println("<input type=\"checkbox\" name=\"test\" value=\"123\">wert 1</input>");
                out.println("<input type=\"checkbox\" name=\"test\" value=\"124\">wert 2</input>");
                out.println("<input type=\"submit\" value=\"Ok\"></input>");
                out.println("</form>");
                out.println("</body>");
                out.println("</html>");
            }
        } catch (NumberFormatException e) {
            out.println("<pre>"+e.toString() +"</pre>");
        } catch (Exception e) {
            out.println("<pre>"+e.toString() +"</pre>");
        } finally {
            out.close();
        }
    }

    private String getTopics() {
        String result = "";
        // TODO : DB Zugriff hier
        try {
            JSONObject categories = new JSONObject(
                    "{{id:1,name:'Personal Information',len:3},"
                    + "{id:2,name:'Income',len:2},"
                    + "{id:3,name:'Family',len:2},"
                    + "{id:4,name:'Child I',len:3}}");
            result = categories.toString();
        } catch (JSONException ee) {
            log.error("could not create json object", ee.toString());
            result = "Error: " + ee.toString();
        }
        return result;
    }
    
    private String askEngine(CarneadesServiceManager manager) {
        String result = "";        
        // creating query
        Statement query = null;
        String kb = "foo.xml";
        List<Statement> answers = new ArrayList<Statement>();
        // ask
        CarneadesMessage msg = manager.askEngine(query, kb, answers);
        // evaluate answer
        if (MessageType.ASKUSER.equals(msg.getType())) {
            // new question
            Question q = QuestionHelper.getQuestionFromStatement(msg.getMessage());
            try {
                JSONObject question = QuestionHelper.getJSONFromQuestion(q);
                result = question.toString();
            } catch (JSONException e) {
                log.error("could not create json object", e.toString());
                result = "Error: " + e.toString();
            }

        } else if (MessageType.SOLUTION.equals(msg.getType())) {
            // solution
            // TODO : implement solution case
            result = "solution found";
        } else {
            // error
            log.error("unknown message type received from ask", msg);            
            result = "Error: unknown message type received from ask";
        }
        
        return result;
    }
    
    private String questions(int category) {
        String result = "";
        // TODO : DB Zugriff hier
        if (category == 1) { // Personal Information
            try {
                JSONObject questions = new JSONObject(
                        "{{id:1,type:'text',question:'Forename: ',answers:['']},"
                        + "{id:2,type:'text',question:'Last name: ',answers:['']},"
                        + "{id:3,type:'select',question:'Country: ',answers:['Austria', 'Bulgarian (&#1073;&#1098;&#1083;&#1075;&#1072;&#1088;&#1089;&#1082;&#1080; &#1077;&#1079;&#1080;&#1082;)','Germany (Deutschland)','Polish (Polski)']}},");
                result = questions.toString();
            } catch (JSONException ee) {
                log.error("could not create json object", ee.toString());
                result = "Error: " + ee.toString();
            }
        } else if (category == 2) { // Income
            try {
                JSONObject questions = new JSONObject(
                        "{{id:1,type:'int',question:'Your last Month-Income: ',answers:['']},"
                        + "{id:2,type:'int',question:'Your estimated income this year: ',answers:['']},");
                result = questions.toString();
            } catch (JSONException ee) {
                log.error("could not create json object", ee.toString());
                result = "Error: " + ee.toString();
            }
        } else if (category == 3) { // Family
            try {
                JSONObject questions = new JSONObject(
                        "{{id:1,type:'radio',question:'family status: ',answers:['not married',married','divorced']},"
                        + "{id:2,type:'int',question:'How many children do you have: ',answers:['']},");
                result = questions.toString();
            } catch (JSONException ee) {
                log.error("could not create json object", ee.toString());
                result = "Error: " + ee.toString();
            }
        } else if (category == 3) { // Child I
            try {
                JSONObject questions = new JSONObject(
                        "{{id:1,type:'text',question:'Child name: ',answers:['']},"
                        + "{{id:2,type:'date',question:'When was your Child born: ',answers:['']},"
                        + "{{id:3,type:'int',question:'Your childs last Month-Income: ',answers:['']},"
                        + "{id:4,type:'int',question:'Your childs estimated income this year: ',answers:['']},");
                result = questions.toString();
            } catch (JSONException ee) {
                log.error("could not create json object", ee.toString());
                result = "Error: " + ee.toString();
            }
        }
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
