/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.fokus.carneades.carneadeswebgui;

import clojure.lang.Keyword;
import clojure.lang.RT;
import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.util.List;
import java.util.Map;
import java.util.logging.Logger;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;
import org.apache.commons.fileupload.FileItem;
import org.apache.commons.fileupload.FileItemFactory;
import org.apache.commons.fileupload.FileUploadException;
import org.apache.commons.fileupload.disk.DiskFileItemFactory;
import org.apache.commons.fileupload.servlet.ServletFileUpload;
import org.fokus.carneades.carneadeswebgui.clojure.NS;


/**
 *
 * @author stb
 */
public class CarneadesServlet extends HttpServlet {

    private static final Logger log = Logger.getLogger(CarneadesServlet.class.getName());
    
    private static final String LKIF_STRING = "LKIF_STRING";

    public CarneadesServlet() {
        super();
        try  {
            // loading carneades libraries
            log.info("loading clojure/main.clj");
            RT.loadResourceScript("clojure/main.clj");
            log.info("loading lkif.clj");
            RT.loadResourceScript("carneades/engine/lkif.clj");
            log.info("loading mapcomponent/export.clj");
            RT.loadResourceScript("carneades/mapcomponent/export.clj"); 
        } catch (Exception e) {
            log.severe(e.getMessage());
        }
    }       
    
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
        // TODO  
        try {
            HttpSession session = request.getSession();
            response.setContentType("text/xml;charset=UTF-8");
            PrintWriter out = response.getWriter();
            String type = request.getParameter("type");
            log.fine("GET".concat(type));
            if("svg".equals(type)) {
                // GET SVG                                      
                // importing lkif    
                // get lkif from session
                String lkifString = (String)session.getAttribute(LKIF_STRING);
                if(lkifString != null) {
                    log.info("loading lkif : ".concat(Integer.toString(lkifString.length())));
                    byte[] lkifBytes = lkifString.getBytes("UTF-8");
                    InputStream lkifStream = new ByteArrayInputStream(lkifBytes);
                    Map lkif = (Map) RT.var(NS.LKIF, "import-lkif").invoke(lkifStream);
                    // get first argument graph
                    log.info("get arg graphs");
                    List argGraphs = (List) lkif.get(Keyword.intern("ags"));
                    log.info("get first graph");
                    Map ag = (Map)argGraphs.get(0);
                    // statement format function
                    Object stmtStr = RT.var(NS.STATEMENT, "statement-formatted").fn();
                    log.info("stmt-frmt fn created");
                    // get options
                    Keyword layoutKW = Keyword.intern("layout");
                    String layoutStr = request.getParameter("layout").toLowerCase();
                    Keyword radialKW = Keyword.intern(layoutStr);
                    Keyword treeifyKW = Keyword.intern("treeify");
                    String treeifyStr = request.getParameter("treeify");
                    boolean treeify = "true".equals(treeifyStr);
                    // convert graph to svg                                  
                    InputStreamReader svgReader = (InputStreamReader)RT.var(NS.MAP, "export-ag-os").invoke(ag, stmtStr, layoutKW, radialKW, treeifyKW, treeify);
                    log.info("svg created");
                    BufferedReader svgBuffer = new BufferedReader(svgReader);
                    while(svgBuffer.ready()) {
                        out.println(svgBuffer.readLine());
                    }
                } else {
                    out.println("");
                }
                out.close(); 
            } else if ("lkif".equals(type)) {
                // GET LKIF
                String lkif = (String)session.getAttribute(LKIF_STRING);                
                log.fine("lkif GET ".concat(Integer.toString(lkif.length())));
                out.println(lkif);                     
                out.close();
                
            }
        } catch(Exception e) {
            e.printStackTrace();
        }
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

        try {
            
            log.info("POST request in Web GUI Servlet");
            // System.out.println("POST request in Web GUI Servlet");
            
            String responseString = "";
            
            boolean isMultipart = ServletFileUpload.isMultipartContent(request);
            if(isMultipart) {
                
                HttpSession session = request.getSession();
                
                log.fine("request is file upload");
                
                // get the uploaded file
                FileItemFactory factory = new DiskFileItemFactory();
                ServletFileUpload upload = new ServletFileUpload(factory);
                List<FileItem> items = upload.parseRequest(request);
                
                // parse the file items
                String fileName = "";
                for(FileItem item : items) {
                    if(!item.isFormField()) {
                        // process file upload
                        fileName = item.getName();
                        String contentType = item.getContentType();
                        log.fine("file name    : ".concat(fileName));
                        log.fine("content type : ".concat(contentType));
                        InputStream inStream = item.getInputStream();
                        InputStreamReader inReader = new InputStreamReader(inStream, "UTF-8");
                        BufferedReader inBuffer = new BufferedReader(inReader);
                        StringBuilder lkifBuffer = new StringBuilder();
                        while(inBuffer.ready()) {
                            lkifBuffer.append(inBuffer.readLine());
                            lkifBuffer.append("\n");
                        }
                        String lkifString = lkifBuffer.toString();
                        session.setAttribute(LKIF_STRING, lkifString);
                        responseString = lkifString;
                    } else {
                        // process form fiels
                    }
                }
                
            } else {
                log.severe("request is not a file upload");
            }
            response.setContentType("text/plain;charset=UTF-8");
            PrintWriter out = response.getWriter();            
            try {
                out.println(responseString);                            
            } catch (Exception e) {
                log.severe(e.getMessage());
                out.println("<pre>"+e.toString() +"</pre>");
            } finally {
                out.close();
            }
        } catch (FileUploadException e) {
            
        } catch(Exception e) {
        
        } finally {            
            
        }
    }

    /** 
     * Returns a short description of the servlet.
     * @return a String containing servlet description
     */
    @Override
    public String getServletInfo() {
        return "Short description";
    }
    
}
