/*
Carneades Argumentation Library and Tools.
Copyright (C) 2008 Matthias Grabmair

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License version 3 (GPL-3)
as published by the Free Software Foundation.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for details.
 
You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/


package Carneades.Control;

// General Imports
import javafx.ext.swing.*;
import java.io.File;
import java.lang.System;
import javax.swing.UIManager;
// import com.sun.java.swing.plaf.nimbus.NimbusLookAndFeel;

// View Imports
import Carneades.Graph.*;
import Carneades.Graph.Elements.Elements.*;

// Model Imports
import Carneades.Argument.Argument;
import Carneades.Argument.Argument.*;
import Carneades.Argument.ArgumentFile;


// Other Control Imports
import Carneades.Control.*;


//--------------------------------

// 1. CONSTRUCT ARGUMENT GRAPH

// Argument Graph

//UIManager.setLookAndFeel("com.sun.java.swing.plaf.nimbus.NimbusLookAndFeel");
UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());

// 2. DECLARE VIEW AND CONTROL COMPONENTS

var control: GraphControl;

control = GraphControl {}

// FINAL DISPLAY

control.newGraph();

control.updateAll(); // update the control and view

control.frame;
