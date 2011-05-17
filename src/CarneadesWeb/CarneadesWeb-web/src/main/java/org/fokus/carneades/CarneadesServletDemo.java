/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.fokus.carneades;

import java.io.IOException;
import java.io.PrintWriter;
import java.lang.reflect.UndeclaredThrowableException;
import java.util.List;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import org.fokus.carneades.simulation.GoogleTranslate;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 *
 * @author stb
 */
public class CarneadesServletDemo extends HttpServlet {

    private static final Logger log = LoggerFactory.getLogger(CarneadesServletDemo.class);
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
        String a = request.getParameter("answers");
        
        List<String> languages = GoogleTranslate.getLanguages();
        for(String l : languages) {
            log.info(l);
        }

        // OUTPUT
        response.setContentType("text/html;charset=UTF-8");
        PrintWriter out = response.getWriter();
        String outMsg = "";
        try {
            // if no answers given with the request               
                
                // TODO : getting answers
          //      outMsg = askEngine();

          //      out.println(outMsg);
            out.println("foo bar");

            
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
    
    private String askEngine() {
        String result = "";        
        log.info("sending questions to user - DEMO");
        result = "{\"questions\" : ["
            + "{\"id\":1,\"type\":\"text\",\"question\":\"Forename: \",\"answers\":[\"\"],\"category\":\"Personal Information\", \"hint\":\"enter your full first name\"},"
            + "{\"id\":2,\"type\":\"text\",\"question\":\"Last name: \",\"answers\":[\"\"],\"category\":\"Personal Information\", \"hint\":\"enter your full family name\"},"
            + "{\"id\":3,\"type\":\"select\",\"question\":\"Country: \",\"answers\":[\"Austria\", \"Bulgarian (&#1073;&#1098;&#1083;&#1075;&#1072;&#1088;&#1089;&#1082;&#1080; &#1077;&#1079;&#1080;&#1082;)\",\"Germany (Deutschland)\",\"Polish (Polski)\"],\"category\":\"Personal Information\", \"hint\":\"where do you life\"},"
            + "{\"id\":4,\"type\":\"radio\",\"question\":\"family status: \",\"answers\":[\"not married\",\"married\",\"divorced\"],\"category\":\"Family\", \"hint\":\"\"},"
            + "{\"id\":5,\"type\":\"int\",\"question\":\"Number of children: \",\"answers\":[\"\"],\"category\":\"Family\", \"hint\":\"Please enter the number of children.\",\"optional\":true},"
            + "{\"id\":6,\"type\":\"date\",\"question\":\"Birthday: \",\"answers\":[\"\"],\"category\":\"Family\"},"
            + "{\"id\":7,\"type\":\"checkbox\",\"question\":\"Hobbies: \",\"answers\":[\"Paragliding\",\"boongie jumping\",\"sharkhunting\",\"jackass-like-stuns\",\"rocket science\"],\"category\":\"Personal Information\",\"hint:\":\"Please be honest.\"}"
            + "]}";
        return result;
    }
    
    /*
    private List<Statement> handleRequestAnswers(String a) {
        List<Statement> result = new ArrayList<Statement>();
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
