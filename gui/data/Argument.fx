
import java.io.IOException;
import java.lang.System;
import javax.swing.*;
import java.awt.Component;
import java.awt.FlowLayout;
import java.util.Map;
import java.util.HashMap;

import edu.uci.ics.jung.graph.Graph;
import edu.uci.ics.jung.graph.Vertex;
import edu.uci.ics.jung.visualization.PluggableRenderer;
import edu.uci.ics.jung.visualization.contrib.DAGLayout;
import edu.uci.ics.jung.visualization.VisualizationViewer;
import edu.uci.ics.jung.graph.impl.SparseGraph;
import edu.uci.ics.jung.graph.impl.DirectedSparseEdge;
import edu.uci.ics.jung.graph.impl.DirectedSparseVertex;
import edu.uci.ics.jung.utils.UserDataContainer.CopyAction.Clone;

import javafx.ui.*;
import javafx.ui.canvas.*;



class Scheme {
	attribute id: String;
}

class Statement {
	attribute id: String;                 // key or label
	attribute content: String;            
	attribute status: String = "stated";  // "stated", "questioned", "accepted", "rejected"
	attribute standard: ProofStandard;
	attribute acceptable: Boolean = false;
	// url, formula, ...
}

class Premise {
	attribute statement: Statement;
	attribute role: String = "";
	attribute negative: Boolean = false;
}

class Exception extends Premise {}
class Assumption extends Premise {}

class Argument {
	attribute id: String;
	attribute scheme: Scheme;
	attribute premises: Premise[];  
	attribute pro: Boolean = true;
	attribute conclusion: Statement;
	attribute defensible: Boolean = false;  // valid
}

class ProofStandard {
	attribute negated: Boolean = false;
	attribute complement: Boolean = false;
	attribute greater: function (context: Context, pro: Argument[], con: Argument[]): Boolean;

	function apply (context: Context, pro: Argument[], con: Argument[]) : Boolean {
		var r = if (not complement) greater(context, pro, con) else greater(context, con, pro);
		if (negated) not r else r;
	}
}


// Context: combines argument graph and context of the Scheme version
class Context {
 	private attribute graph: Graph = new SparseGraph();
	private attribute statementVertices: Map = new HashMap(); // statement -> statement Vertex
	private attribute argumentVertices: Map = new HashMap();  // argument -> argument Vertex

	attribute prior: function (a1: Argument, a2: Argument) : Boolean =
		function (a1,a2) { false }
		
	// put forward an argument. "assert" is an FX keyword.
	// to do: check whether the graph would become cyclic before asserting the argument.
	function put (arg: Argument) : Void {
		// add the argument vertex
		var argNode: Vertex;
		if (argumentVertices.containsKey(arg.id)) { 
			return // warning or raise exception?
		} else {
			argNode = new DirectedSparseVertex();	
			graph.addVertex(argNode);
			argNode.setUserDatum("argument", arg, new Clone);
			argumentVertices.put(arg.id, argNode);
		}
		
		// add a vertex for each premise, together with the 
		// edges from the premises to the argument
		for (p in arg.premises) {
			var pv: Vertex;
			if (not statementVertices.containsKey(p.statement.id)) {
				pv = new DirectedSparseVertex();
				graph.addVertex(pv);
				pv.setUserDatum("statement", p.statement, new Clone);
				statementVertices.put(p.statement.id, pv);
			} else {
				pv = statementVertices.get(p.statement.id) as Vertex;
			}
			graph.addEdge(new DirectedSparseEdge(pv,argNode));
		}
		
		// add the vertex for the conclusion and
		// the edge from the argument to its conclusion

		var cv : Vertex;
		if (not statementVertices.containsKey(arg.conclusion.id)) {
			cv = new DirectedSparseVertex();
			graph.addVertex(cv);
			cv.setUserDatum("statement", arg.conclusion, new Clone);
			statementVertices.put(arg.conclusion.id, cv);
		} else {
			cv = statementVertices.get(arg.conclusion.id) as Vertex;
		}
		graph.addEdge(new DirectedSparseEdge(argNode,cv));
	}
	
	function getGraph () { graph }
	
}

// Some boilerplate code for Swing components in JavaFX.

private class SwingWidget extends Widget {
  attribute awtComponent: Component;
  
  function createComponent() {
    var newComponent: JComponent = new JPanel();
    newComponent.setLayout(new FlowLayout());
    newComponent.add(awtComponent);
    return newComponent;
  }
}

var c = Context {}

// key list
var s1 = Statement { id: "s1", content: "All men are mortal."}
var s2 = Statement { id: "s2", content: "Socrates is a man."}
var s3 = Statement { id: "s3", content: "Socrates is fictive."}
var s4 = Statement { id: "s4", content: "Socrates is mortal."}

var a1 = Argument { 
	id: "arg1" 
	premises: [ Premise { statement: s1},
				Assumption { statement: s2},
				Exception { statement: s3}]
	conclusion: s4
}

c.put(a1);
var g = c.getGraph();
var pv = new PluggableRenderer();
var sl = new DAGLayout(g);
var vv = new VisualizationViewer(sl,pv);

Frame {
    title: "Argument Graph"
    visible: true
    content: SwingWidget { awtComponent: vv }
}
