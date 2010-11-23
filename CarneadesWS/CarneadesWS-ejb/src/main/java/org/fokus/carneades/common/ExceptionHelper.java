/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.fokus.carneades.common;

/**
 *
 * @author stb
 */
public class ExceptionHelper {
    
    public static Throwable skipRuntimeExceptions(RuntimeException e) {
        Throwable cause = e.getCause();
        if(cause instanceof RuntimeException) {
            return skipRuntimeExceptions((RuntimeException)cause);
        }
        return cause;
    }

}
