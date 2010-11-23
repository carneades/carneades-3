/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.fokus.carneades;

import clojure.lang.Fn;
import clojure.lang.Keyword;
import clojure.lang.RT;
import clojure.lang.Symbol;
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
import org.fokus.carneades.common.ExceptionHelper;
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
            log.info("loading clojure files finished");
        } catch(Exception e) {
            log.error(e.toString());
        }
    }

    public CarneadesMessage askEngine(Statement query, String kb, List<Statement> answers) {

        CarneadesMessage cm = null;

        log.info("starting engine with kb: "+kb);
        log.info("query: " + query.toString());

        this.answers.addAll(answers);

        try {
            // importing lkif
            log.info("loading lkif");
            Map lkif = (Map) RT.var("carneades.engine.lkif", "lkif-import").invoke(kb);

            // constructing arguments
            List argGraphs = (List) lkif.get(Keyword.intern("ags"));
            log.info("creating goal");
            List goal = (List) RT.var("clojure.core", "list").invoke(Symbol.intern("p"), Symbol.intern("?x"));
            log.info("creating lkif generator");
            Fn lkifGen = (Fn) RT.var("carneades.engine.lkif", "generate-arguments-from-lkif").invoke(lkif);
            log.info("creating askable? function");
            Askable askFn = new Askable();
            log.info("creating answers");
            List<List> cljAnswers = ClojureUtil.getSeqFromStatementList(this.answers);
            GetAnswer getAnswerFn = new GetAnswer(cljAnswers);
            log.info("creating ask generator");
            Fn askGen = (Fn) RT.var("carneades.engine.ask", "ask-user").invoke(askFn, getAnswerFn);
            log.info("combining generators");
            List generators = (List) RT.var("clojure.core", "list").invoke(askGen, lkifGen);
            List solutions;
            if (this.state == null) {
                log.info("starting engine for the first time");
                Map ag = (Map) argGraphs.get(0);
                solutions = (List) RT.var("clojure.core", "doall").invoke(RT.var("carneades.engine.shell", "construct-arguments").invoke(goal, 50, ag, generators));
            } else {
                log.info("starting engine again");
                solutions = (List) RT.var("clojure.core", "doall").invoke(RT.var("carneades.engine.shell", "continue-construction").invoke(goal, 50, this.state, generators));
            }
            // solution found
            log.info("solution found: " + solutions.getClass().getName());
            log.info("uniting solutions");
            Map solAG = (Map) RT.var("carneades.engine.shell", "unite-solutions").invoke(solutions);
            log.info("creating CarneadesMessage");
            cm = new CarneadesMessage();
            cm.setMessage(query);
            cm.setAG(solAG);
            cm.setType(MessageType.SOLUTION);
        } catch (RuntimeException e) {
            Throwable cause = ExceptionHelper.skipRuntimeExceptions(e);
            if (cause instanceof AskException) {
                // asking user
                AskException ae = (AskException)cause;
                log.info("question from engine");
                Statement subgoal = ClojureUtil.getStatementFromSeq(ae.getGoal());
                this.state = ae.getState();
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

            return cm;
            
        }
        
    }
    
    private static void handleStandardError(Exception e) {
        log.error("Error during argumentation construction: " + e.getClass().getName() + " " + e.getCause().getMessage());
        e.printStackTrace();
    }

}
