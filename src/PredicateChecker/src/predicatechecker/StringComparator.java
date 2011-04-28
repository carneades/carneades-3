/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package predicatechecker;

import java.util.Comparator;

/**
 *
 * @author stb
 */
public class StringComparator implements Comparator<String>{

    public int compare(String s1, String s2) {
        return s1.toLowerCase().compareTo(s2.toLowerCase());
    }

}
