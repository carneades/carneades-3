package org.fokus.carneades.common;


import org.fokus.carneades.CarneadesServiceManager;

/**
 * Utility class for locating and accessing an EJB from diverse clients. 
 * 
 * @author Andreas Penski (andreas.penski@fokus.fraunhofer.de)
 *
 */
public class EjbLocator extends AbstractJndiServiceLocator{

	private static final String APPLICATION_NAME = "CarneadesWS-ear";

		
	public static CarneadesServiceManager getCarneadesService(){
		return (CarneadesServiceManager) getRemoteReference(getRefName(CarneadesServiceManager.class, APPLICATION_NAME));
	}

}
