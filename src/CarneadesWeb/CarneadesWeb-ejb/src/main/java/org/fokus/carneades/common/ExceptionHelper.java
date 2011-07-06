/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.fokus.carneades.common;

/**
 * 
 * Exceptions thrown in Clojure consist of many nested RuntimeExceptions
 * making it difficult to see the root cause. This class provides a static 
 * funtion to get the final root cause of an exception skipping the nested
 * RuntimeExceptions.
 *
 * @author stb
 */
// TODO : ExceptionHelper maybe obsolete?
public class ExceptionHelper {
    
    public static Throwable skipRuntimeExceptions(RuntimeException e) {
        Throwable cause = e.getCause();
        if(cause instanceof RuntimeException) {
            return skipRuntimeExceptions((RuntimeException)cause);
        }
        return cause;
    }

}
