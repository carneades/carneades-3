/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package carneades.editor.uicomponents;

import java.awt.Component;
import java.awt.Dimension;
import java.awt.Insets;
import javax.swing.JPanel;

public class PreviewContainer extends JPanel {

    protected int H_GAP = 16;
    protected int V_GAP = 10;

    @Override
    public Dimension getPreferredSize() {
        int n = getComponentCount();

        if (n == 0) {
            return new Dimension(H_GAP, V_GAP);
        }

        Component comp = getComponent(0);
        Dimension dc = comp.getPreferredSize();

        int w = dc.width;
        int h = dc.height;

        Dimension dp = getParent().getSize();

        int nCol = Math.max((dp.width - H_GAP) / (w + H_GAP), 1);
        int nRow = n / nCol;

        if (nRow * nCol < n) {
            nRow++;
        }

        int ww = nCol * (w + H_GAP) + H_GAP;
        int hh = nRow * (h + V_GAP) + V_GAP;

        Insets ins = getInsets();

        return new Dimension(ww + ins.left + ins.right,
                hh + ins.top + ins.bottom);

    }

    @Override
    public Dimension getMaximumSize() {
        return getPreferredSize();
    }

    @Override
    public Dimension getMinimumSize() {
        return getPreferredSize();
    }

    @Override
    public void doLayout() {
        Insets ins = getInsets();
        int x = ins.left + H_GAP;
        int y = ins.top + V_GAP;

        int n = getComponentCount();

        if (n == 0) {
            return;
        }

        Component comp = getComponent(0);
        Dimension dc = comp.getPreferredSize();

        int w = dc.width;
        int h = dc.height;

        Dimension dp = getParent().getSize();

        int nCol = Math.max((dp.width - H_GAP) / (w + H_GAP), 1);
        int nRow = n / nCol;
        if (nRow * nCol < n) {
            nRow++;
        }

        int index = 0;
        for (int k = 0; k < nRow; k++) {
            for (int m = 0; m < nCol; m++) {
                if (index >= n) {
                    return;
                }
                comp = getComponent(index++);
                comp.setBounds(x, y, w, h);
                x += w + H_GAP;
            }
            y += h + V_GAP;
            x = ins.left + H_GAP;
        }
    }
}
