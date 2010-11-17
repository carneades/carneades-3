/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.fokus.carneades;

import javax.ejb.Stateless;
import javax.jws.WebService;
import org.fokus.carneades.api.CarneadesMessage;
import org.fokus.carneades.api.Statement;

/**
 *
 * @author stb
 */

@Stateless
@WebService(endpointInterface="org.fokus.carneades.CarneadesService")
public class CarneadesServiceManager implements CarneadesService{

    public CarneadesMessage askEngine(String s) {
        CarneadesMessage cm = new CarneadesMessage();
        Statement stmt = new Statement();
        stmt.setPredicate(s);
        stmt.getArgs().add("test");
        cm.setMessage(stmt);
        return cm;
    }



}
