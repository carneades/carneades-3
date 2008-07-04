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


package GraphSketch1.Control;

// General Imports
//import javafx.ext.swing.*;
//import javafx.scene.paint.*;
import javafx.gui.*;
import java.lang.System;
import java.io.File;

// View Imports
import GraphSketch1.*;
import GraphSketch1.Graph.*;
import GraphSketch1.Graph.Elements.Elements.*;

// Model Imports
import GraphSketch1.Argument.Argument;
import GraphSketch1.Argument.Argument.*;
import GraphSketch1.Argument.ArgumentFile;

// Other Control Imports
import GraphSketch1.Control.*;

//--------------------------------

// 1. CONSTRUCT ARGUMENT GRAPH

// Argument Graph

//var argumentGraph: ArgumentGraph = ArgumentFile.getGraphFromFile("data/test2.xml");


var argumentGraph: ArgumentGraph = ArgumentGraph { id: "testgraph" }

argumentGraph = ArgumentFile.getGraphFromFile(new File("data/socrates.xml"));

// Statements
/*
var s1: Statement = Statement { id: "Conclusion", wff: "Socrates is mortal." }
var s2: Statement = Statement { id: "Mortality", wff: "Humans are mortal.", standard: BestArgument {} }
var s3: Statement = Statement { id: "Socrates", wff: "Socrates is human.", standard: Scintilla {}, assumption: true }
insert s1 into argumentGraph.statements;
insert s2 into argumentGraph.statements;
insert s3 into argumentGraph.statements;

// Premises
var p1: Premise = Premise { statement: s2, exception: true }
var p2: Premise = Premise { statement: s3 }

// Arguments
var a1: Argument = Argument { 
	id: "a1"
	conclusion: s1
}
var a2: Argument = Argument { 
	id: "a2"
	conclusion: s3
}

insert p1 into a1.premises;
insert p2 into a1.premises;

insert a1 into argumentGraph.arguments;
insert a2 into argumentGraph.arguments;*/

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

