/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package carneades.editor.uicomponents;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Image;
import java.awt.Insets;
import javax.swing.JPanel;
import javax.swing.border.MatteBorder;

/**
 *
 * @author pal
 */
public class PagePreview extends JPanel {

    protected int m_w;
    protected int m_h;
    protected Image m_source;
    protected Image m_img;

    public PagePreview(int w, int h, Image source) {
        m_w = w;
        m_h = h;
        m_source = source;
        m_img = m_source.getScaledInstance(m_w, m_h,
                Image.SCALE_SMOOTH);
        m_img.flush();
        setBackground(Color.white);
        setBorder(new MatteBorder(1, 1, 2, 2, Color.black));

    }

    public void setScaledSize(int w, int h) {
        m_w = w;
        m_h = h;
        m_img = m_source.getScaledInstance(m_w, m_h,
                Image.SCALE_SMOOTH);
        repaint();
    }

    @Override
    public Dimension getPreferredSize() {
        Insets ins = getInsets();
        return new Dimension(m_w + ins.left + ins.right,
                m_h + ins.top + ins.bottom);
    }

    @Override
    public Dimension getMaximumSize() {
        return getPreferredSize();
    }

    @Override
    public Dimension getMinimumSize() {
        return getPreferredSize();
    }

    public void paint(Graphics g) {
        g.setColor(getBackground());
        g.fillRect(0, 0, getWidth(), getHeight());
        g.drawImage(m_img, 0, 0, this);
        paintBorder(g);
    }
}
