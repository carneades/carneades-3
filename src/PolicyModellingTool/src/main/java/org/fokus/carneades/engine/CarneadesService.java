/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.fokus.carneades.engine;

import java.util.List;
import org.fokus.carneades.api.CarneadesMessage;
import org.fokus.carneades.api.Statement;

/**
 *
 * @author stb
 * 
 * The ejb interface for the Carneades Engine Service. 
 * Uses the Carneades API to hide Clojure data types
 * 
 */

public interface CarneadesService {

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
    CarneadesMessage askEngine(Statement query, String kb, List<String> askables, List<Statement> answers);
    
    /**
     * accept and reject statements in an argument graph
     * 
     * @param argGraph the path to a lkif file containing an argument graph
     * @param accepts a list of strings representing the policy proposals to be accepted; (valid proposal1), (valid propsal2), ...
     * @param rejects a list of strings representing the policy proposals to be rejected; (valid proposal1), (valid propsal2), ...
     * @return path to a lkif file containing the evaluated argument graph
     */
    CarneadesMessage evaluateArgGraph(String argGraph, List<String> accepts, List<String> rejects);
    
    /**
     * extract policy rules from an argument graph
     * 
     * @param argGraph path to lkif file containing an argument graph
     * @return returns a list of statements containing the policy proposals; (valid proposal1), (valid propsal2)
     */
    CarneadesMessage getPolicyRules(String argGraph);
    
    /**
     * visualize alkif argument graph as svg
     * 
     * @param argGraph path to a lkif file containing an argument graph
     * @param height height of svg
     * @param width width of svg
     * @return path to svg file
     */
    CarneadesMessage getSVGFromGraph(String argGraph);

}
