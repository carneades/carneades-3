/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package predicatechecker;

import java.io.File;
import javax.swing.filechooser.FileFilter;

/**
 *
 * @author stb
 */
public class SimpleFilter extends FileFilter{

    private String[] okExt;
    private String description;

    public SimpleFilter(String desc, String[] ext) {
        this.okExt = ext;
        this.description = desc;
    }

    public boolean accept(File f) {
        if(okExt != null) {
            if(f.isDirectory()) {
                return true;
            }
            for(String e : okExt) {
                if(f.getName().toLowerCase().endsWith(e)) {
                    return true;
                }
            }
            return false;
        } else {
            return true;
        }
    }

    public String getDescription() {
        return this.description;
    }

}
