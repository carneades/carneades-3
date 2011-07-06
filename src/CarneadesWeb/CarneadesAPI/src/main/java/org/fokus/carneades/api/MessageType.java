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

    ASKUSER, // question during construction process to be answered by the user
    RULES, // policy rules contained in an arguemnt graph
    GRAPH, // lkif argument graph
    SVG, // graphical visualization of an argument graph in svg
    SOLUTION; // solution of argument construction process

    public String value() {
        return name();
    }

    public static MessageType fromValue(String v) {
        return valueOf(v);
    }

}
