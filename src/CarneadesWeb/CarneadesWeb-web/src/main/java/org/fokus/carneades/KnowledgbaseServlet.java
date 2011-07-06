/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.fokus.carneades;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.StringTokenizer;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * 
 * Servlet class to simulate CMS. Enable to load lkif files located
 * locally under /tmp/ via an url.
 *
 * @author stb
 */

// TODO : Servlet should be replaced by CMS
public class KnowledgbaseServlet extends HttpServlet {
    
    private static final Logger log = LoggerFactory.getLogger(KnowledgbaseServlet.class);
   
    /** 
     * Processes requests for both HTTP <code>GET</code> and <code>POST</code> methods.
     * @param request servlet request
     * @param response servlet response
     * @throws ServletException if a servlet-specific error occurs
     * @throws IOException if an I/O error occurs
     */
    protected void processRequest(HttpServletRequest request, HttpServletResponse response)
            throws ServletException, IOException {
        
        response.setContentType("text;charset=UTF-8");
        PrintWriter out = response.getWriter();        
        try {
            log.debug("in the kb servlet now");
            String result = null;
            String query = request.getPathInfo();
            
            log.info("getting knowledgebase : "+query);
            
            StringTokenizer t = new StringTokenizer(query, "/");

            if (t.countTokens() == 1) {
                result = returnKB(t);
            } else {
                result = "#ERROR# incorrect format of request";
            }
            //resp.setCharacterEncoding("UTF-8");
            //resp.getWriter().write(result);
            out.println(result);
        } finally {
            out.close();
        }
    } 
    
    private String returnKB(StringTokenizer t) {
        
        String rulePart = null;
        String result = "";
        String prePath = "/tmp/";        
	if (t.hasMoreTokens()) {
		rulePart =  t.nextToken();
	}
        String kbPath = prePath.concat(rulePart);
        // TODO : make some DB request here        
        try {
            File f = new File(kbPath);
            log.debug(f.getAbsolutePath());
            BufferedReader fIn = new BufferedReader(new FileReader(f));
            String l;
            while ((l = fIn.readLine()) != null) {
                result = result.concat(l);
            }
            fIn.close();
            log.debug(result);
        } catch (IOException e) {
            log.error(e.toString());
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
