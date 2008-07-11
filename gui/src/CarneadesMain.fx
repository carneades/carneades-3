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
import java.lang.System;
import javafx.ext.swing.*;
import javafx.scene.paint.*;
import java.io.File;

// View Imports
import Carneades.*;
import Carneades.Graph.*;
import Carneades.Graph.Elements.Elements.*;

// Model Imports
import Carneades.Argument.Argument;
import Carneades.Argument.Argument.*;
import Carneades.Argument.ArgumentFile;

// Other Control Imports
import Carneades.Control.*;

//System.setProperty("apple.laf.useScreenMenuBar", "true");

//--------------------------------

// 1. CONSTRUCT ARGUMENT GRAPH

// Argument Graph

//var argumentGraph: ArgumentGraph = ArgumentFile.getGraphFromFile("data/test2.xml");


var argumentGraph: ArgumentGraph = ArgumentGraph { id: "testgraph" }

//var argumentGraph = ArgumentFile.getGraphFromFile(new File("data/socrates.xml"));

// 2. DECLARE VIEW AND CONTROL COMPONENTS

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
	width: GC.appWidth
	height: GC.appHeight
	control: bind control
	visible: true
}

// 4. SET UP CONTROL

control = GraphControl {
	// As long as the model does not provide its own full alteration methods, 
	// we need to inverse-bind the graph to the controller.
	argumentGraph: bind argumentGraph with inverse
	frame: bind frame
	layout: bind layout
}

// FINAL DISPLAY

control.updateAll(); // update the control and view

frame;

