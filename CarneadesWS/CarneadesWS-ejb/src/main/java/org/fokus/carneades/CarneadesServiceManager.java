/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.fokus.carneades;

import clojure.lang.Fn;
import clojure.lang.Keyword;
import clojure.lang.LazySeq;
import clojure.lang.PersistentArrayMap;
import clojure.lang.PersistentList;
import clojure.lang.PersistentStructMap;
import clojure.lang.RT;
import clojure.lang.Symbol;
import java.util.ArrayList;
import java.util.List;
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

    private PersistentStructMap state = null;
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
        log.info("query: "+query.toString());

        this.answers.addAll(answers);

        try{
            // importing lkif
            PersistentArrayMap lkif = (PersistentArrayMap)RT.var("carneades.engine.lkif", "lkif-import").invoke(kb);

            // constructing arguments
            LazySeq argGraphs = (LazySeq)lkif.get(Keyword.intern("ags"));            
            PersistentList goal = (PersistentList)RT.var("clojure.core","list").invoke(Symbol.intern("p"), Symbol.intern("?x"));
            Fn lkifGen = (Fn)RT.var("carneades.engine.lkif", "generate-arguments-from-lkif").invoke(lkif);
            Askable askFn = new Askable();
            List<LazySeq> cljAnswers = ClojureUtil.getSeqFromStatementList(this.answers);
            GetAnswer getAnswerFn = new GetAnswer(cljAnswers);
            Fn askGen = (Fn)RT.var("carneades.engine.ask","").invoke(askFn, getAnswerFn);
            PersistentList generators = (PersistentList)RT.var("clojure.core", "list").invoke(askGen, lkifGen);
            LazySeq solutions;
            if(this.state == null){
                PersistentStructMap ag = (PersistentStructMap)argGraphs.first();
                solutions = (LazySeq)RT.var("carneades.engine.shell", "construct-arguments").invoke(goal, 50, ag, generators);
            } else {
                solutions = (LazySeq)RT.var("carneades.engine.shell", "continue-construction").invoke(goal, 50, this.state, generators);
            }
            log.info("solution found");
            PersistentStructMap solAG = (PersistentStructMap)RT.var("carneades.engine.shell", "unite-solutions").invoke(solutions);
            cm = new CarneadesMessage();
            cm.setMessage(query);
            cm.setAG(solAG);
            cm.setType(MessageType.SOLUTION);
        }catch(AskException e) {
            log.info("question from engine");
            Statement goal = ClojureUtil.getStatementFromSeq(e.getGoal());
            this.state = e.getState();
            cm = new CarneadesMessage();
            cm.setMessage(goal);
            cm.setAG(null);
            cm.setType(MessageType.ASKUSER);
        }catch (Exception e) {
            log.error("Error during argumentation construction");            
        }

        return cm;
        
    }

}
