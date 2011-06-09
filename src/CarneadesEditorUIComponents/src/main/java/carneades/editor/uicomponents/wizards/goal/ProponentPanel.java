/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

/*
 * ProponentPanel.java
 *
 * Created on Oct 19, 2010, 3:43:25 PM
 */

package carneades.editor.uicomponents.wizards.goal;

/**
 *
 * @author pal
 */
public class ProponentPanel extends javax.swing.JPanel {

    /** Creates new form ProponentPanel */
    public ProponentPanel() {
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

        positiveNegativeGroup = new javax.swing.ButtonGroup();
        inOutGroup = new javax.swing.ButtonGroup();
        jLabel1 = new javax.swing.JLabel();
        jLabel2 = new javax.swing.JLabel();
        inRadioButton = new javax.swing.JRadioButton();
        outRadioButton = new javax.swing.JRadioButton();
        positiveRadioButton = new javax.swing.JRadioButton();
        negativeRadioButton = new javax.swing.JRadioButton();
        jLabel3 = new javax.swing.JLabel();

        jLabel1.setText("The main issue is:");

        mainIssueTextArea.setBackground(new java.awt.Color(222, 222, 222));
        mainIssueTextArea.setColumns(20);
        mainIssueTextArea.setEditable(false);
        mainIssueTextArea.setLineWrap(true);
        mainIssueTextArea.setRows(5);
        mainIssueTextArea.setWrapStyleWord(true);
        mainIssueTextArea.setMinimumSize(new java.awt.Dimension(0, 50));
        mainIssueTextArea.setPreferredSize(new java.awt.Dimension(260, 100));

        jLabel2.setText("I want the...");

        inOutGroup.add(inRadioButton);
        inRadioButton.setSelected(true);
        inRadioButton.setText("in");
        inRadioButton.setName("in"); // NOI18N

        inOutGroup.add(outRadioButton);
        outRadioButton.setText("out");
        outRadioButton.setName("out"); // NOI18N

        positiveNegativeGroup.add(positiveRadioButton);
        positiveRadioButton.setSelected(true);
        positiveRadioButton.setText("positive");
        positiveRadioButton.setName("positive"); // NOI18N

        positiveNegativeGroup.add(negativeRadioButton);
        negativeRadioButton.setText("negative");
        negativeRadioButton.setName("negative"); // NOI18N

        jLabel3.setText("to be...");

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(this);
        this.setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(layout.createSequentialGroup()
                        .addComponent(mainIssueTextArea, javax.swing.GroupLayout.DEFAULT_SIZE, 381, Short.MAX_VALUE)
                        .addContainerGap())
                    .addGroup(layout.createSequentialGroup()
                        .addComponent(jLabel1)
                        .addContainerGap(279, Short.MAX_VALUE))
                    .addComponent(jLabel2)
                    .addGroup(layout.createSequentialGroup()
                        .addComponent(positiveRadioButton)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(negativeRadioButton)
                        .addContainerGap(235, Short.MAX_VALUE))
                    .addGroup(layout.createSequentialGroup()
                        .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addGroup(layout.createSequentialGroup()
                                .addComponent(inRadioButton)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                .addComponent(outRadioButton))
                            .addGroup(layout.createSequentialGroup()
                                .addComponent(jLabel3)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, 41, javax.swing.GroupLayout.PREFERRED_SIZE)))
                        .addContainerGap(307, Short.MAX_VALUE))))
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addGap(24, 24, 24)
                .addComponent(jLabel1)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(mainIssueTextArea, javax.swing.GroupLayout.PREFERRED_SIZE, 86, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addGap(18, 18, 18)
                .addComponent(jLabel2)
                .addGap(11, 11, 11)
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(positiveRadioButton)
                    .addComponent(negativeRadioButton))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(jLabel3)
                .addGap(4, 4, 4)
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(outRadioButton)
                    .addComponent(inRadioButton))
                .addContainerGap(64, Short.MAX_VALUE))
        );
    }// </editor-fold>//GEN-END:initComponents


    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.ButtonGroup inOutGroup;
    private javax.swing.JRadioButton inRadioButton;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JLabel jLabel2;
    private javax.swing.JLabel jLabel3;
    public final javax.swing.JTextArea mainIssueTextArea = new javax.swing.JTextArea();
    private javax.swing.JRadioButton negativeRadioButton;
    private javax.swing.JRadioButton outRadioButton;
    private javax.swing.ButtonGroup positiveNegativeGroup;
    private javax.swing.JRadioButton positiveRadioButton;
    // End of variables declaration//GEN-END:variables

}
