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

//var argumentGraph: ArgumentGraph;

//var argumentGraph = ArgumentFile.getGraphFromFile(new File("examples/PiersonPost.xml"));

// 2. DECLARE VIEW AND CONTROL COMPONENTS

var argumentGraph: ArgumentGraph;
var layout: GraphLayout;
var graph: Graph;
var control: GraphControl;
var frame: GraphFrame;

// 3. SET UP VIEW

graph = CarneadesGraph {
	argumentGraph: bind argumentGraph
	control: bind control
}

layout = TreeLayout {
	graph: graph
	width: GC.appWidth
	height: GC.appHeight
}

frame = GraphFrame {
	graph: bind graph
	argumentGraph: bind argumentGraph
	layout: bind layout
	control: bind control
	visible: true
}

// 4. SET UP CONTROL

control = GraphControl {
	// As long as the model does not provide its own full alteration methods, 
	// we need to inverse-bind the graph to the controller.
	argumentGraph: bind argumentGraph with inverse
	graph: bind graph with inverse
	frame: bind frame
	layout: bind layout
}

// FINAL DISPLAY

control.newGraph();

control.updateAll(); // update the control and view

