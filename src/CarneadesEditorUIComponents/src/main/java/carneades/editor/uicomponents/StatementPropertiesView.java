/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

/*
 * StatementPropertiesView.java
 *
 * Created on Aug 27, 2010, 11:49:02 AM
 */

package carneades.editor.uicomponents;

import java.awt.Color;
import javax.swing.UIManager;

/**
 *
 * @author pal
 */
public class StatementPropertiesView extends javax.swing.JPanel {

    /** Creates new form StatementPropertiesView */
    public StatementPropertiesView() {
        initComponents();
    }

    /** This method is called from within the constructor to
     * initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is
     * always regenerated by the Form Editor.
     */
    @SuppressWarnings("unchecked")
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        statusGroup = new javax.swing.ButtonGroup();
        jLabel4 = new javax.swing.JLabel();
        jLabel5 = new javax.swing.JLabel();
        jSeparator1 = new javax.swing.JSeparator();
        jPanel1 = new javax.swing.JPanel();
        jLabel6 = new javax.swing.JLabel();
        jLabel2 = new javax.swing.JLabel();
        jLabel3 = new javax.swing.JLabel();
        jScrollPane1 = new javax.swing.JScrollPane();
        jLabel1 = new javax.swing.JLabel();

        jLabel4.setText("Graph:");

        mapTitleText.setBackground(new java.awt.Color(222, 222, 222));
        mapTitleText.setEditable(false);
        mapTitleText.setDisabledTextColor(new java.awt.Color(1, 1, 1));

        jLabel5.setText("Path:");

        pathText.setBackground(new java.awt.Color(222, 222, 222));
        pathText.setEditable(false);
        pathText.setDisabledTextColor(new java.awt.Color(226, 210, 196));

        complementAcceptableText.setBackground(new java.awt.Color(222, 222, 222));
        complementAcceptableText.setText("Complement Acceptable");

        acceptableText.setBackground(new java.awt.Color(222, 222, 222));
        acceptableText.setText("Acceptable");

        jLabel6.setText("Acceptability:");

        proofstandardComboBox.setModel(new javax.swing.DefaultComboBoxModel(new String[] { "Dialectical Validity", "Scintilla of Evidence", "Preponderance of Evidence", "Clear and Convincing Evidence", "Beyond Reasonable Doubt" }));

        jLabel2.setText("Proof standard:");

        statusComboBox.setModel(new javax.swing.DefaultComboBoxModel(new String[] { "Stated", "Questioned", "Accepted", "Rejected" }));

        jLabel3.setText("Status:");

        statementTextArea.setColumns(20);
        statementTextArea.setLineWrap(true);
        statementTextArea.setRows(5);
        statementTextArea.setWrapStyleWord(true);
        statementTextArea.setMinimumSize(new java.awt.Dimension(15, 17));
        statementTextArea.setPreferredSize(new java.awt.Dimension(180, 85));
        jScrollPane1.setViewportView(statementTextArea);

        javax.swing.GroupLayout jPanel1Layout = new javax.swing.GroupLayout(jPanel1);
        jPanel1.setLayout(jPanel1Layout);
        jPanel1Layout.setHorizontalGroup(
            jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel1Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(proofstandardComboBox, 0, 317, Short.MAX_VALUE)
                    .addComponent(jLabel2)
                    .addGroup(jPanel1Layout.createSequentialGroup()
                        .addComponent(jLabel3)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(statusComboBox, 0, 259, Short.MAX_VALUE))
                    .addGroup(jPanel1Layout.createSequentialGroup()
                        .addComponent(jLabel6)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addComponent(complementAcceptableText, javax.swing.GroupLayout.Alignment.TRAILING, javax.swing.GroupLayout.DEFAULT_SIZE, 220, Short.MAX_VALUE)
                            .addComponent(acceptableText, javax.swing.GroupLayout.DEFAULT_SIZE, 220, Short.MAX_VALUE))))
                .addContainerGap())
            .addComponent(jScrollPane1, javax.swing.GroupLayout.Alignment.TRAILING, javax.swing.GroupLayout.DEFAULT_SIZE, 341, Short.MAX_VALUE)
        );
        jPanel1Layout.setVerticalGroup(
            jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanel1Layout.createSequentialGroup()
                .addComponent(jScrollPane1, javax.swing.GroupLayout.DEFAULT_SIZE, 270, Short.MAX_VALUE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(statusComboBox, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jLabel3))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jLabel2)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(proofstandardComboBox, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(acceptableText, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jLabel6))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(complementAcceptableText, javax.swing.GroupLayout.PREFERRED_SIZE, 27, javax.swing.GroupLayout.PREFERRED_SIZE))
        );

        jLabel1.setText("Statement:");

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(this);
        this.setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING, false)
                    .addComponent(jSeparator1, javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jLabel1, javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(javax.swing.GroupLayout.Alignment.LEADING, layout.createSequentialGroup()
                        .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addComponent(jLabel5)
                            .addComponent(jLabel4))
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING, false)
                            .addComponent(mapTitleText)
                            .addComponent(pathText, javax.swing.GroupLayout.DEFAULT_SIZE, 234, Short.MAX_VALUE))))
                .addContainerGap(40, Short.MAX_VALUE))
            .addComponent(jPanel1, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(pathText, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jLabel5))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(mapTitleText, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jLabel4))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jSeparator1, javax.swing.GroupLayout.PREFERRED_SIZE, 8, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jLabel1)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jPanel1, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );
    }// </editor-fold>//GEN-END:initComponents


    // Variables declaration - do not modify//GEN-BEGIN:variables
    public final javax.swing.JTextField acceptableText = new javax.swing.JTextField();
    public final javax.swing.JTextField complementAcceptableText = new javax.swing.JTextField();
    private javax.swing.JLabel jLabel1;
    private javax.swing.JLabel jLabel2;
    private javax.swing.JLabel jLabel3;
    private javax.swing.JLabel jLabel4;
    private javax.swing.JLabel jLabel5;
    private javax.swing.JLabel jLabel6;
    private javax.swing.JPanel jPanel1;
    private javax.swing.JScrollPane jScrollPane1;
    private javax.swing.JSeparator jSeparator1;
    public final javax.swing.JTextField mapTitleText = new javax.swing.JTextField();
    public final javax.swing.JTextField pathText = new javax.swing.JTextField();
    public final javax.swing.JComboBox proofstandardComboBox = new javax.swing.JComboBox();
    public final javax.swing.JTextArea statementTextArea = new javax.swing.JTextArea();
    public final javax.swing.JComboBox statusComboBox = new javax.swing.JComboBox();
    private javax.swing.ButtonGroup statusGroup;
    // End of variables declaration//GEN-END:variables

    // our modifications:
    public static StatementPropertiesView viewInstance = new StatementPropertiesView();

    public static synchronized StatementPropertiesView instance()
    {
        return viewInstance;
    }

    public static synchronized void reset()
    {
        viewInstance = new StatementPropertiesView();
    }
}
