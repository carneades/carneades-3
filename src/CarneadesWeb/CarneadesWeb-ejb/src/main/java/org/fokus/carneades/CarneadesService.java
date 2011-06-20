/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.fokus.carneades;

import java.util.List;
import javax.ejb.Remote;
import org.fokus.carneades.api.CarneadesMessage;
import org.fokus.carneades.api.Statement;

/**
 *
 * @author stb
 */

@Remote
public interface CarneadesService {

    CarneadesMessage askEngine(Statement query, String kb, List<String> askables, List<Statement> answers);
    
    CarneadesMessage evaluateArgGraph(String argGraph, List<String> accepts, List<String> rejects);
    
    CarneadesMessage getPolicyRules(String argGraph);
    
    CarneadesMessage getSVGFromGraph(String argGraph, int height, int width);

}
