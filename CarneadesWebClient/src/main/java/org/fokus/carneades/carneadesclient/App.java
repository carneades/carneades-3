package org.fokus.carneades.carneadesclient;

import org.fokus.carneades.CarneadesService;
import org.fokus.carneades.CarneadesServiceManagerService;

/**
 * Hello world!
 *
 */
public class App 
{
    public static void main( String[] args )
    {
        CarneadesServiceManagerService cs = new CarneadesServiceManagerService();

        CarneadesService s = cs.getCarneadesServiceManagerPort();

        System.out.println(s.askEngine("foo"));
    }
}
