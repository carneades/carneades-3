/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.fokus.carneades.api;

/**
 *
 * @author stb
 */
public enum MessageType {

    ASKUSER,
    SOLUTION;

    public String value() {
        return name();
    }

    public static MessageType fromValue(String v) {
        return valueOf(v);
    }

}
