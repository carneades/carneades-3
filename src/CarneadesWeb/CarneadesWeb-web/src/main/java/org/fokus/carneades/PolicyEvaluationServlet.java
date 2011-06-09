/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.fokus.carneades;

import java.io.File;
import java.io.IOException;
import java.io.PrintWriter;
import java.net.URLDecoder;
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
import org.fokus.carneades.simulation.Translator;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 *
 * @author stb
 */
public class PolicyEvaluationServlet extends HttpServlet {
    
    private static final Logger log = LoggerFactory.getLogger(PolicyEvaluationServlet.class);
    private HttpSession session;

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
            
            log.info("request in Policy Evaluation Servlet");
            
            // INPUT
            session = request.getSession();
            // incoming request
            String jsonINString = URLDecoder.decode(request.getParameter("json"),"UTF-8");
            JSONObject jsonIN = new JSONObject(jsonINString);
            
            log.info("jsonIN : "+jsonINString);
            
            // OUTPUT
            JSONObject jsonOUT = new JSONObject();
            
            // Carneades Engine
            log.info("getting CarneadesService");
            CarneadesService service = EjbLocator.getCarneadesService();
            
            if(jsonIN.has("policyrules")) {                
                jsonOUT = handlePolicyRules(service, jsonIN.getString("policyrules"));
            } else if(jsonIN.has("showgraph")) {
                jsonOUT = handleShowGraph(service, jsonIN.getString("showgraph"));
            } else if(jsonIN.has("evaluate")) {
                jsonOUT = handleEvaluate(service, jsonIN.getJSONObject("evaluate"));                
            } else if (jsonIN.has("abduction")) {
                jsonOUT = handleAbduction(service, jsonIN.getJSONObject("abduction"));
            } else {
                jsonOUT.put("error", "unknown input: "+jsonINString);
            }
            
            
            // Sending OUTPUT
            response.setContentType("text/html;charset=UTF-8");
            PrintWriter out = response.getWriter();
            try {
                out.println(jsonOUT.toString());                            
            } catch (Exception e) {
                e.printStackTrace();
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

    private JSONObject handleEvaluate(CarneadesService service, JSONObject jsonEvaluate) throws JSONException{        
        
        log.info("handleEvaluate");
        
        JSONObject r = new JSONObject();
        
        String argGraph = jsonEvaluate.getString("argGraph");
        JSONArray jsonAccept = jsonEvaluate.getJSONArray("accept");
        JSONArray jsonReject = jsonEvaluate.getJSONArray("reject");
        
        List<String> acceptIDs = new ArrayList<String>();
        for(int i=0; i<jsonAccept.length(); i++) {
            acceptIDs.add(jsonAccept.getString(i));
        }
        
        List<String> rejectIDs = new ArrayList<String>();
        for (int i = 0; i < jsonReject.length(); i++) {
            rejectIDs.add(jsonReject.getString(i));
        }

        CarneadesMessage cm = service.evaluateArgGraph(argGraph, acceptIDs, rejectIDs);

        if (MessageType.GRAPH.equals(cm.getType())) {
            // solution            
            // Statement solution = cm.getMessage();
            String evaluatedGraph = cm.getAG();
            log.info("sending evaluated graph to user: " + evaluatedGraph);
            r.put("evaluated", evaluatedGraph);
        } else {
            r.put("error", "unexpected message type (GRAPH expected) : "+cm.getType().name());
        }
        

        return r;
                
    }

    private JSONObject handleAbduction(CarneadesService service, JSONObject jSONObject) throws JSONException{
        
        // TODO : implement handleAbduction
        
        return null;
        
    }

    private JSONObject handlePolicyRules(CarneadesService service, String lkifPath) throws JSONException {
                        
        log.info("handlePolicyRules");
        
        JSONObject o = new JSONObject();
        
        log.info("call service with path: "+lkifPath);
        CarneadesMessage cm = service.getPolicyRules(lkifPath);
        
        if(MessageType.RULES.equals(cm.getType())) {
            
            JSONArray schemesArray = new JSONArray();
            for(Statement stmt : cm.getStatements()) {
                String policyRule = stmt.getArgs().get(0);
                //JSONObject stmtJSON = new JSONObject();
                //stmtJSON.put("rule", policyRule);
                schemesArray.put(policyRule);
            }
            o.put("policyrules", schemesArray);
            
        } else {
            o.put("error", "unexpected message type (RULES expected) : "+cm.getType().name());
        }
        
        return o;
        
    }

    private JSONObject handleShowGraph(CarneadesService service, String agPath) throws JSONException {
        
        JSONObject o = new JSONObject();
        
        log.info("showGraph : "+agPath);
        
        CarneadesMessage cm = service.getSVGFromGraph(agPath);
        if(MessageType.SVG.equals(cm.getType())) {
            String localPath = cm.getAG();
            File f = new File(localPath);            
            String webPath = "http://localhost:8080/CarneadesWeb-web/svg/"+f.getName();
            o.put("graphpath", webPath);
        } else {
            o.put("error", "unexpected message type (SVG expected) : "+cm.getType().name());
        }
        
        return o;        
        
    }
}
