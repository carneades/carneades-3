/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.fokus.carneades;

import org.fokus.carneades.clojureutil.NS;
import clojure.lang.IFn;
import clojure.lang.Keyword;
import clojure.lang.RT;
import clojure.lang.Symbol;
import java.io.BufferedReader;
import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import javax.ejb.Stateful;
import org.fokus.carneades.Fn.AskException;
import org.fokus.carneades.Fn.AskHandler;
import org.fokus.carneades.Fn.Askable;
import org.fokus.carneades.api.CarneadesMessage;
import org.fokus.carneades.api.MessageType;
import org.fokus.carneades.api.Statement;
import org.fokus.carneades.clojureutil.ClojureUtil;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 *
 * @author stb
 * 
 * implementation of the CarneadesService interface
 * 
 */

@Stateful
public class CarneadesServiceManager implements CarneadesService{

    // logging
    private static final Logger log = LoggerFactory.getLogger(CarneadesServiceManager.class);

    /** 
     * state variables of the engine
     */    
    // current search state
    private Map state = null;
    // list of total answers
    private List<Statement> answers = new ArrayList<Statement>();
    // query as clojure list
    private List goal = null;
    // query as carneades.api.Statement
    private Statement query = null;
    // current question from engine
    private List lastQuestion = null;
    // promise used for communication to the engine
    private IFn toEngine = null;
    // promise used for communication from the engine
    private IFn fromEngine = null;

    /**
     * constructor 
     * compiles clojure files
     */
    public CarneadesServiceManager() {
        log.info("constructing stateful session bean");
        try {
            // loading scripts
            log.info("loading lkif.clj");
            RT.loadResourceScript("carneades/engine/lkif.clj");
            log.info("loading ask.clj");
            RT.loadResourceScript("carneades/engine/ask.clj");
            // RT.loadResourceScript("carneades/engine/argument-search.clj");
            log.info("loading shell.clj");
            RT.loadResourceScript("carneades/engine/shell.clj");
            //log.info("loading viewer.clj");
            //RT.loadResourceScript("carneades/ui/diagram/viewer.clj");
            log.info("loading clojure/main.clj");
            RT.loadResourceScript("clojure/main.clj");
            log.info("loading mapcomponent/export.clj");
            RT.loadResourceScript("carneades/mapcomponent/export.clj");
            //log.info("loading json.clj");
            //RT.loadResourceScript("clojure/contrib/json.clj");
            log.info("loading clojure files finished");
        } catch(Exception e) {
            log.error(e.toString());
        }
    }

    /**
     * visualize alkif argument graph as svg
     * 
     * @param argGraph path to a lkif file containing an argument graph
     * @param height height of svg
     * @param width width of svg
     * @return path to svg file
     */
    public CarneadesMessage getSVGFromGraph(String argGraph) {        
        
        CarneadesMessage cm = new CarneadesMessage();
        
        try {
            
            log.info("get svg from lkif") ;
            
            // importing lkif
            log.info("loading lkif");
            Map lkif = (Map) RT.var(NS.LKIF, "import-lkif").invoke(argGraph);
            log.info("get arg graphs");
            List argGraphs = (List) lkif.get(Keyword.intern("ags"));
            log.info("get first graph");
            Map ag = (Map)argGraphs.get(0);
            
            // get output file 
            // TODO : replace with CMS operations
            int c = 0;
            String prepath = "/tmp/";
            String svgPath = prepath + "graph0.svg";
            File f = new File(svgPath);
            while(f.exists()) {
                c++;
                svgPath = prepath + "graph" + Integer.toString(c) + ".svg";            
                f = new File(svgPath);
            } 
            
            log.info("svg path : "+svgPath);
            
            Object stmtStr = RT.var(NS.STATEMENT, "statement-formatted").fn();
            log.info("stmt-frmt fn created");
            
            // convert graph to svg
            // TODO : use options for export     
            Keyword layoutKW = Keyword.intern("layout");
            Keyword radialKW = Keyword.intern("hierarchical");
            //Keyword heightKW = Keyword.intern("height");
            //Keyword widthKW = Keyword.intern("width");
            Keyword treeifyKW = Keyword.intern("treeify");
            RT.var(NS.MAP, "export-ag").invoke(ag, stmtStr, svgPath, layoutKW, radialKW, treeifyKW, true);
            log.info("svg created");
            
            cm.setAG(svgPath);
            cm.setType(MessageType.SVG);
            
        } catch (Exception e) {
            handleStandardError(e);
        } finally {
            return cm;
        }
    }
    
    
    /**
     * extract policy rules from an argument graph
     * 
     * @param argGraph path to lkif file containing an argument graph
     * @return returns a list of statements containing the policy proposals; (valid proposal1), (valid propsal2)
     */
    public CarneadesMessage getPolicyRules(String argGraph) {
        
        CarneadesMessage cm = new CarneadesMessage();
        
        try {
            
            // importing lkif
            log.info("loading lkif");
            Map lkif = (Map) RT.var(NS.LKIF, "import-lkif").invoke(argGraph);
            log.info("get arg graphs");
            List argGraphs = (List) lkif.get(Keyword.intern("ags"));
            log.info("get first graph");
            Map ag = (Map)argGraphs.get(0);
            
            log.info("get \"valid\" nodes");
            List validNodes = (List)RT.var(NS.ARGUMENT, "get-nodes").invoke(ag, Symbol.intern("valid"));
            log.info("get \"valid\" statements");
            List validStmts = (List)RT.var(NS.CORE, "map").invoke(Keyword.intern("statement"),validNodes);
            
            List<Statement> policyRules = new ArrayList<Statement>();
            // log.info("start argument loop");
            for(Object o : validStmts) {
                //log.info(o.toString());
                List stmtSExpr = (List)o;
                Statement stmt = ClojureUtil.getStatementFromSeq(stmtSExpr) ;
                // log.info("scheme : "+scheme);
                policyRules.add(stmt);
            }
            
            log.info("found "+Integer.toString(policyRules.size()) + " policy rules");
            
            cm.setStatements(policyRules);
            cm.setType(MessageType.RULES);
            
        } catch (Exception e) {
            handleStandardError(e);
        } finally {
            return cm;
        }
    }
    
    
    /**
     * accept and reject statements in an argument graph
     * 
     * @param argGraph the path to a lkif file containing an argument graph
     * @param accepts a list of strings representing the policy proposals to be accepted; (valid proposal1), (valid propsal2), ...
     * @param rejects a list of strings representing the policy proposals to be rejected; (valid proposal1), (valid propsal2), ...
     * @return path to a lkif file containing the evaluated argument graph
     */
    public CarneadesMessage evaluateArgGraph(String argGraph, List<String> accepts, List<String> rejects) {
        
        CarneadesMessage cm = null;
        
        try {
            
            log.info("evaluating") ;
                    
            // importing lkif
            log.info("loading lkif");
            Map lkif = (Map) RT.var(NS.LKIF, "import-lkif").invoke(argGraph);
            List argGraphs = (List) lkif.get(Keyword.intern("ags"));
            Map ag = (Map)argGraphs.get(0);
            

            // evaluate
            // accept
            List<Statement> accStmts = new ArrayList<Statement>();
            for(String a : accepts) {
                Statement s = new Statement();
                s.setPredicate("valid");
                s.getArgs().add(a);
                accStmts.add(s);
                log.info("acceptable stmt: "+a);
            }
            List accSExpr = ClojureUtil.getSeqFromStatementList(accStmts); 
            Map accAG = (Map)RT.var(NS.ARGUMENT, "accept").invoke(ag, accSExpr);
            
            // reject
            List<Statement> rejStmts = new ArrayList<Statement>();
            for(String r : rejects) {
                Statement s = new Statement();
                s.setPredicate("valid");
                s.getArgs().add(r);
                rejStmts.add(s);
                log.info("rejectable stmt: "+r);
            }
            List rejSExpr = ClojureUtil.getSeqFromStatementList(rejStmts);
            Map evaluatedAG = (Map)RT.var(NS.ARGUMENT, "reject").invoke(accAG, rejSExpr);
                     
            
            // save & return evaluated graph
            String evaluatePath = storeArgGraph(evaluatedAG);
            log.info("evaluated graph:"+evaluatePath);
            cm = new CarneadesMessage();
            cm.setAG(evaluatePath);            
            cm.setType(MessageType.GRAPH);
        
        } catch (Exception e) {
            handleStandardError(e);
        } finally { 
            log.info("sending Carneades Message back");
            return cm;
        }
    }
    
    
    /**
     * construct arguments for the query with rules from kb. Interrupts for questions
     * 
     * @param query The main-issue to construct arguments pro and con this query.
     * @param kb The path to the knowledge base containing the rules to construct arguments.
     * @param askables A list of predicates where the engine will interrupt the argument construction process to ask for sub goals.
     * @param answers A list of already answered statements from the last question
     * @return returns either the final argument graph or a question
     * 
     */ 
    public CarneadesMessage askEngine(Statement query, String kb, List<String> askables, List<Statement> answers2) {

        CarneadesMessage cm = null;

        try {
            
            // checking new and old answers
            if (answers2 != null) {
                log.info("number of new answers: " + Integer.toString(answers2.size()));
                this.answers.addAll(answers2);
            } else {
                log.info("no new answers");
            }
            log.info("number of total answers: " + Integer.toString(this.answers.size()));

            // creating new ask handler
            log.info("creating answers", this.answers);
            List<List> cljAnswers = ClojureUtil.getSeqFromStatementList(this.answers);
            log.info("creating answer function");
            AskHandler askHandler = new AskHandler(cljAnswers);

            // start or continue engine
            if (state == null) {
                cm = startEngine(query, kb, askables, askHandler);
            } else {
                cm = continueEngine(askHandler);
            }

        } catch (Exception e) {
            log.error(e.getMessage());
        } finally {
            log.info("sending Carneades Message back");
            return cm;
        }
        
    }
    
    // start engine for the first time
    private CarneadesMessage startEngine(Statement query, String kb, List<String> askables, AskHandler askHandler) {
        
        CarneadesMessage cm = null;

        log.info("starting engine with kb: "+kb);
        log.info("query: " + query.toString());
        this.query = query;
        
        try {
            // importing lkif
            log.info("loading lkif");
            Map lkif = (Map) RT.var(NS.LKIF, "import-lkif").invoke(kb);

            // getting goal            
            log.info("creating goal");
            // TODO : the goal has to be general
            this.goal = ClojureUtil.getSeqFromStatement(query);//(List) RT.var("clojure.core", "list").invoke(Symbol.intern("p"), Symbol.intern("?x"));
                        
            // creating promises
            toEngine = (IFn)RT.var(NS.CORE, "promise").invoke();
            fromEngine = (IFn)RT.var(NS.CORE, "promise").invoke();
            
            // creating generators
            log.info("creating lkif generator");
            IFn lkifGen = (IFn) RT.var(NS.LKIF, "generate-arguments-from-lkif").invoke(lkif);            
            log.info("generators as list");            
            List generators = (List) RT.var(NS.CORE, "list").invoke(lkifGen);
            
            // askable function
            log.info("creating askable? function");
            Askable askableFn = new Askable(askables);
                        
            // start engine
            log.info("starting engine for the first time");
            List argGraphs = (List) lkif.get(Keyword.intern("ags"));
            System.out.println(argGraphs);
            Map ag;
            if (argGraphs == null) {
                ag = (Map) RT.var(NS.ARGUMENT, "*empty-argument-graph*").get();
            } else {
                ag = (Map) argGraphs.get(0);
            }
            System.out.println(ag);
            
            //RT.var(NS.CORE, "doall").invoke(
            RT.var(NS.SHELL, "future-construction").invoke(toEngine, fromEngine);
            
            // sending first request
            // TODO : max nodes and max turns
            int maxTurns = 2;
            int maxNodes = 50;
            Object msg = RT.var(NS.CORE, "list").invoke(this.goal, maxNodes, maxTurns, ag, generators, askableFn);
            cm = communicateWithEngine(msg, askHandler);
            
   
        } catch (Exception e) {
            e.printStackTrace();
            handleStandardError(e);
        } finally {            
            return cm;            
        }
    }
    
    // continue with current construction that has been interrupted by asking a question
    private CarneadesMessage continueEngine(AskHandler askHandler) {
        
        CarneadesMessage cm = null;
        
        try {
        
            // sending further request
            log.info("continue with engine");           
            Object msg = askHandler.getAnswer(this.lastQuestion, this.state);
            cm = communicateWithEngine(msg, askHandler);
            
             
        } catch (Exception e) {
            handleStandardError(e);
        } finally {        
            return cm;
        }
        
    }
    
    // using promises to send and receive messages ro and from the engine
    private CarneadesMessage communicateWithEngine(Object msg, AskHandler askHandler) {
        
        CarneadesMessage cm = null;
        
        try {
            
            // sending message
            log.info("sending request to engine", msg);
            RT.var(NS.CORE, "deliver").invoke(this.toEngine, msg);
            
            // read response
            // TODO : may block here !
            log.info("waiting for answer from engine");
            List o = (List)RT.var(NS.CORE, "deref").invoke(this.fromEngine);            
            log.info("answer received from engine: " + Integer.toString(o.size()));
            
            // log.info((String)RT.var(NS.CORE,"pr-str").invoke(o));
            // RT.var(NS.CORE, "println").invoke(o);
            
            // check for solution or ask
            boolean isSolution = (Boolean)RT.var(NS.CORE, "=").invoke(Symbol.intern("solution"), o.get(0));
            boolean isAsk = (Boolean)RT.var(NS.CORE, "=").invoke(Symbol.intern("ask"), o.get(0));
            
            log.info("is solution? : "+Boolean.toString(isSolution));
            log.info("is ask? : "+Boolean.toString(isAsk));
            
            if(isSolution) {
                
                // solution found                
                List solutions = (List)o.get(1);
                int solNr = (Integer)RT.var(NS.CORE, "count").invoke(solutions);
                log.info("solution: " + Integer.toString(solNr) + " - "+solutions.getClass().getName() );
                // TODO : handle all solutions
                // get last solution for substitution
                Map lastSol = (Map)solutions.get(solutions.size()-1);
                Map lastSubs = (Map)lastSol.get(Keyword.intern("substitutions"));
                List lastSolStmt = (List) RT.var(NS.UNIFY, "apply-substitution").invoke(lastSubs,this.goal);
                log.info("uniting solutions");
                Map solAG = (Map) RT.var(NS.CORE,"doall").invoke(RT.var(NS.SHELL, "unite-solutions").invoke(solutions));
                solAG = (Map)RT.var(NS.CORE, "assoc").invoke(solAG, Keyword.intern("main-issue"), lastSolStmt);
                log.info("serializing argument graph");                
                //PrintWriter jsonWriter = new PrintWriter(new StringWriter());
                //String jsonString = "";
                //RT.var(NS.JSON, "write-json") .invoke(solAG, jsonWriter);
                // {:ags (solAG)}
                //Map lkifMap = (Map)RT.map(Keyword.intern("ags"),RT.var(NS.CORE, "list").invoke(solAG));
                //RT.var(NS.LKIF,"export-lkif").invoke(lkifMap, lkifWriter);
                //RT.var(NS.CORE, "println").invoke(lkifMap);
                //jsonString = jsonWriter.toString();
                //log.info(lkifString);
                String agPath = storeArgGraph(solAG);
                //String ag = (String)RT.var(NS.JSON,"json-str").invoke(solAG);
                //log.info(jsonString);
                log.info(agPath);
                log.info("creating CarneadesMessage");
                cm = new CarneadesMessage();
                cm.setMessage(ClojureUtil.getStatementFromSeq(lastSolStmt));
                //cm.setAG(jsonString);
                cm.setAG(agPath);
                cm.setType(MessageType.SOLUTION);
                
            } else if (isAsk) {
                
                // question raised
                log.info("question from engine");
                this.lastQuestion = (List)o.get(1);
                this.state = (Map)o.get(2);
                this.toEngine = (IFn)o.get(3);
                this.fromEngine = (IFn)o.get(4);
                
                // checking if question has already been answered
                try {
                    // already answered
                    Object answer = askHandler.getAnswer(this.lastQuestion, this.state);
                    cm = communicateWithEngine(answer, askHandler);
                } catch(AskException e) {
                    // ask user
                    Statement subgoal = ClojureUtil.getStatementFromSeq(this.lastQuestion);                
                    cm = new CarneadesMessage();
                    cm.setMessage(subgoal);
                    cm.setAG(null);
                    cm.setType(MessageType.ASKUSER);
                }
                

            } else {
                log.info("unknown answer from engine");
            }

        
            
        } catch (Exception e) {
            e.printStackTrace();
            handleStandardError(e);
        } finally {        
            return cm;
        }
        
    }
    
    private void handleStandardError(Exception e) {
        log.error("Error during argumentation construction: " + e.getClass().getName() + " " + e.getCause().getMessage());
        e.printStackTrace();
    }
    
    private String storeArgGraph(Map argGraph) throws Exception{
        
        // TODO : replace with CMS
        
        log.info("storing argument graph");
        
        int c = 0;
        String prepath = "/tmp/";
        String path = prepath + "graph0.lkif";
        File f = new File(path);
        while(f.exists()) {
            c++;
            path = prepath + "graph" + Integer.toString(c) + ".lkif";            
            f = new File(path);
        }          
        
        storeArgGraph(argGraph, path);
        
        return path;
        
    }
    
    private void storeArgGraph(Map argGraph, String path) throws Exception{        
        log.info(path);                
        // {:ags (solAG)}
        Map lkifMap = (Map)RT.map(Keyword.intern("ags"),RT.var(NS.CORE, "list").invoke(argGraph));
        RT.var(NS.LKIF,"export-lkif").invoke(lkifMap, path);        
    }

}
