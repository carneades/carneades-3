/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.fokus.carneades;

import javax.ejb.Remote;
import javax.jws.WebMethod;
import javax.jws.WebService;
import org.fokus.carneades.api.CarneadesMessage;

/**
 *
 * @author stb
 */

@Remote
@WebService
public interface CarneadesService {

    @WebMethod CarneadesMessage askEngine(String s);

}
