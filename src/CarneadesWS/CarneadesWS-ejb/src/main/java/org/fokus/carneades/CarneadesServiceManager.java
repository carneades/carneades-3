/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.fokus.carneades;

import clojure.lang.AFn;
import clojure.lang.Fn;
import clojure.lang.Keyword;
import clojure.lang.RT;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import javax.ejb.Stateful;
import org.fokus.carneades.Fn.AskException;
import org.fokus.carneades.Fn.Askable;
import org.fokus.carneades.Fn.GetAnswer;
import org.fokus.carneades.api.CarneadesMessage;
import org.fokus.carneades.api.MessageType;
import org.fokus.carneades.api.Statement;
import org.fokus.carneades.clojureutil.ClojureUtil;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 *
 * @author stb
 */

@Stateful
public class CarneadesServiceManager implements CarneadesService{

    private static final Logger log = LoggerFactory.getLogger(CarneadesServiceManager.class);

    private Map state = null;
    private List<Statement> answers = new ArrayList<Statement>();
    private AFn returnFn = null;

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
            log.info("loading viewer.clj");
            RT.loadResourceScript("carneades/ui/diagram/viewer.clj");
            log.info("loading json.clj");
            RT.loadResourceScript("clojure/contrib/json.clj");
            log.info("loading clojure files finished");
        } catch(Exception e) {
            log.error(e.toString());
        }
    }

    public CarneadesMessage askEngine(Statement query, String kb, List<String> askables, List<Statement> answers2) {

        CarneadesMessage cm = null;

        log.info("starting engine with kb: "+kb);
        log.info("query: " + query.toString());        

        if (answers2 != null) {
            log.info("number of new answers: " + Integer.toString(answers2.size()));
            this.answers.addAll(answers2);            
        } else {
            log.info("no new answers");
        }
        
        log.info("number of total answers: " + Integer.toString(this.answers.size()));

        try {
            // importing lkif
            log.info("loading lkif");
            Map lkif = (Map) RT.var(NS.LKIF, "lkif-import").invoke(kb);

            // constructing arguments
            List argGraphs = (List) lkif.get(Keyword.intern("ags"));
            log.info("creating goal");
            // TODO : the goal has to be general
            List goal = ClojureUtil.getSeqFromStatement(query);//(List) RT.var("clojure.core", "list").invoke(Symbol.intern("p"), Symbol.intern("?x"));
            log.info("creating lkif generator");
            Fn lkifGen = (Fn) RT.var(NS.LKIF, "generate-arguments-from-lkif").invoke(lkif);
            log.info("creating askable? function");
            Askable askFn = new Askable(askables);
            log.info("creating answers", this.answers);
            List<List> cljAnswers = ClojureUtil.getSeqFromStatementList(this.answers);
            log.info("creating answer function");
            GetAnswer getAnswerFn = new GetAnswer(cljAnswers, this.returnFn);
            // log.info("creating ask generator");
            // Fn askGen = (Fn) RT.var(NS.ASK, "ask-user").invoke(askFn, getAnswerFn);
            log.info("combining generators");
            List generators = (List) RT.var(NS.CORE, "list").invoke(lkifGen);
            List solutions;
            if (this.state == null) {
                log.info("starting engine for the first time");
                Map ag; 
                if(argGraphs == null) {
                    ag = (Map)RT.var(NS.ARGUMENT, "*empty-argument-graph*").invoke();
                } else {
                    ag = (Map) argGraphs.get(0);
                }
                solutions = (List) RT.var(NS.CORE, "doall").invoke(RT.var(NS.SHELL, "monadic-construction").invoke(goal, 50, ag, generators, askFn, getAnswerFn));
            } else {
                log.info("starting engine again");
                if(returnFn != null) {       
                    AFn ret = this.returnFn;
                    this.returnFn = null;
                    log.info("continuation found");
                    solutions = (List) RT.var(NS.CORE, "doall").invoke(RT.var(NS.MONADS, "run-cont").invoke(ret.invoke(getAnswerFn)));
                } else {
                    log.info("could not find continuation; starting construction again");
                    solutions = (List) RT.var(NS.CORE, "doall").invoke(RT.var(NS.SHELL, "continue-construction").invoke(goal, 50, this.state, generators));
                }
            }
            // solution found
            int solNr = (Integer)RT.var(NS.CORE, "count").invoke(solutions);
            log.info("solution found: " + Integer.toString(solNr) + " - "+solutions.getClass().getName() );
            Map firstSol = (Map)solutions.get(solutions.size()-1);
            Map firstSubs = (Map)firstSol.get(Keyword.intern("substitutions"));
            List firstSolStmt = (List) RT.var(NS.UNIFY, "apply-substitution").invoke(firstSubs,goal);
            log.info("uniting solutions");
            Map solAG = (Map) RT.var(NS.CORE,"doall").invoke(RT.var(NS.SHELL, "unite-solutions").invoke(solutions));
            solAG = (Map)RT.var(NS.CORE, "assoc").invoke(solAG, Keyword.intern("main-issue"), firstSolStmt);
            log.info("serializing argument graph");
            //PrintWriter jsonWriter = new PrintWriter(new StringWriter());
            //String jsonString = "";
            //RT.var(NS.JSON, "write-json") .invoke(solAG, jsonWriter);
            // {:ags (solAG)}
            //Map lkifMap = (Map)RT.map(Keyword.intern("ags"),RT.var(NS.CORE, "list").invoke(solAG));
            //RT.var(NS.LKIF,"lkif-export").invoke(lkifMap, lkifWriter);
            //RT.var(NS.CORE, "println").invoke(lkifMap);
            //jsonString = jsonWriter.toString();
            //log.info(lkifString);
            String ag = (String)RT.var(NS.JSON,"json-str").invoke(solAG);
            //log.info(jsonString);
            log.info(ag);
            log.info("creating CarneadesMessage");
            cm = new CarneadesMessage();
            cm.setMessage(query);
            //cm.setAG(jsonString);
            cm.setAG(ag);
            cm.setType(MessageType.SOLUTION);
        } catch (RuntimeException e) {
            // e.printStackTrace();
            Throwable cause = (Throwable)RT.var(NS.STACKTRACE,"root-cause").invoke(e);// ExceptionHelper.skipRuntimeExceptions(e);
            if (cause instanceof AskException) {
                // asking user
                AskException ae = (AskException)cause;
                log.info("question from engine");
                Statement subgoal = ClojureUtil.getStatementFromSeq(ae.getGoal());
                this.state = ae.getState();
                this.returnFn = ae.getReturnFn();
                cm = new CarneadesMessage();
                cm.setMessage(subgoal);
                cm.setAG(null);
                cm.setType(MessageType.ASKUSER);
            } else {
                // other runtime exception
                handleStandardError(e);
            }
   
        } catch (Exception e) {
            handleStandardError(e);
        } finally {

            log.info("sending Carneades Message back");
            return cm;
            
        }
        
    }
    
    private static void handleStandardError(Exception e) {
        log.info("Error during argumentation construction: " + e.getClass().getName() + " " + e.getCause().getMessage());
        e.printStackTrace();
    }

}
