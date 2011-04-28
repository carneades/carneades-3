/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.fokus.carneades.translationgui;

import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.StringSelection;
import java.awt.datatransfer.Transferable;
import javax.swing.DefaultListModel;
import javax.swing.JComponent;
import javax.swing.JList;
import javax.swing.JOptionPane;
import javax.swing.TransferHandler;
import javax.swing.TransferHandler.TransferSupport;

/**
 *
 * @author stb
 */
public class ListTransferHandler extends TransferHandler {
    
    private JList transferList;
    
    public ListTransferHandler (JList trList) {
        this.transferList = trList;
    }

    @Override
    public boolean canImport(TransferSupport support) {
        //System.out.println("canImport: "+ support.isDataFlavorSupported(DataFlavor.stringFlavor));
        if(!support.isDataFlavorSupported(DataFlavor.stringFlavor)) {
            return false;
        } else {
            return true;
        }
    }

    @Override
    protected Transferable createTransferable(JComponent c) {
        System.out.println("createTransferable");
        
        return new StringSelection(Integer.toString((Integer)this.transferList.getSelectedValue()));
    }
    
    @Override
    public int getSourceActions(JComponent c) {
        return MOVE;
    }
    
    

    @Override
    public boolean importData(TransferSupport support) {
        System.out.println("importData: "+support);
        if (!support.isDrop()) {
            return false;
        } else {
            System.out.println("isDrop");
            JList targetList = (JList)support.getComponent();
            if(targetList.equals(this.transferList)) {
                System.out.println("equals transferlist");
                System.out.println("get ListModel");
                DefaultListModel listModel = null;
                try {
                    listModel = (DefaultListModel)targetList.getModel();
                } catch (Exception e) {
                    JOptionPane.showMessageDialog(null, "no listModel found for: "+targetList.getModel().getClass().toString()+"\n"+e.getMessage(),"Warnung", JOptionPane.WARNING_MESSAGE);
                }
                System.out.println("get DropLocation");
                JList.DropLocation dl = (JList.DropLocation)support.getDropLocation();
                System.out.println("get index");
                int index = dl.getIndex();
                System.out.println("index = "+Integer.toString(index));
                Transferable t = support.getTransferable();
                System.out.println("get data");
                Integer data;
                try {                    
                    data = Integer.parseInt((String)t.getTransferData(DataFlavor.stringFlavor));
                    System.out.println("data = "+data);
                } catch (Exception e) {
                    e.printStackTrace();
                    return false;
                }
                int oldIndex = listModel.indexOf(data);
                System.out.println("oldIndex = "+Integer.toString(oldIndex));
                if(oldIndex < index) {
                    listModel.add(index, data);
                    listModel.remove(oldIndex);
                } else if (oldIndex > index) {
                    listModel.add(index, data);
                    listModel.remove(oldIndex+1);
                }
                return true;
            } else {
                return false;
            }
        }
    }  
    
}

