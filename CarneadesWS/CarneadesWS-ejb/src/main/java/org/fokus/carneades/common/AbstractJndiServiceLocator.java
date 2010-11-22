package org.fokus.carneades.common;

import java.util.Properties;

import javax.naming.Context;
import javax.naming.InitialContext;
import javax.naming.NamingException;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Abstract base class for ejb lookup via jndi. Default lookup against a local jboss.
 * @author Andreas Penski (andreas.penski@fokus.fraunhofer.de)
 *
 */
public abstract class AbstractJndiServiceLocator {

	private static final Logger log = LoggerFactory.getLogger(AbstractJndiServiceLocator.class);
	
	protected String INITIAL_CONTEXT_FACTORY = "org.jnp.interfaces.NamingContextFactory";
	
	protected String URL_PKG_PREFIXES = "org.jboss.naming:org.jnp.interfaces";
	
	protected String PROVIDER_URL = "jnp://localhost:1099";
	
	private static Context jndiContext;
	
	private static Object getReference(final String serviceName) {
		Object service = null;
		try {
			service = getContext().lookup(serviceName);
		} catch (NamingException e) {
			log.error("Error lookup " + serviceName, e);
		}
		return service;
	}

	protected static Object getRemoteReference(final String service) {
		return getReference(service + "/remote");
	}

	protected static Object getLocalReference(final String service) {
		return getReference(service + "/local"); 
	}

	private static Context getContext() {
		if (jndiContext == null) {
			Properties p = new Properties();
			p.put(Context.INITIAL_CONTEXT_FACTORY,"org.jnp.interfaces.NamingContextFactory");
			p.put(Context.URL_PKG_PREFIXES, "org.jboss.naming:org.jnp.interfaces");
			p.put(Context.PROVIDER_URL,"jnp://localhost:1099");
			try {
				jndiContext = new InitialContext(p);
				
			} catch (NamingException e) {
				log.error("JNDI error",e);
			}
			
		}
		return jndiContext;
	}

	@SuppressWarnings("unchecked")
	protected static String getRefName(Class ejb, String appName){
		return new StringBuilder(appName).append("/").append(ejb.getSimpleName()).toString();
	}
	
	protected static String getRefName(String name, String appName){
		return new StringBuilder(appName).append("/").append(name).toString();
	}
	

}