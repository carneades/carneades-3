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

package Carneades.Graph;

// General imports
import javafx.ext.swing.*;
import javafx.scene.*;
import java.lang.System;

// import the rest of the graph package
import Carneades.Graph.Elements.Elements.*;

// import the necessary parts of the model
import Carneades.Argument.Argument;
import Carneades.Argument.Argument.*;

public class CarneadesGraph extends Graph {

	
	// this attribute is the binding to the model
	public attribute argumentGraph: ArgumentGraph;

	// the invisible root node
	override attribute root = Vertex {
		caption: "root"
		x: bind root.xShift
		y: bind root.yShift
		visible: false
	}

	override attribute vertices = [];

	override attribute edges = [];

	// CONVERSION FUNCTIONS

	public function update(): Void {

		// This function is there because whenever something other than the cardinality of the
		// argument and statement sequences of the model change, no event gets thrown back that
		// triggers the binding reset which is necessary to trigger the new graph to be forwarded
		// to the layout object.
		if (GC.debug) { System.out.println("CarneadesGraph.update()"); }
		
		// destroy the vertex array to allow the garbage collection to clean up
		for (v in vertices) {
			v.parentVertex = null;
			v.children = null;
		}
		// destroy the edge array to allow the garbage collection to clean up
		for (e in edges) {
			e.producer = null;
			e.recipient = null;
		}
		
		root.children = [];
		vertices = [root, toVertices(argumentGraph.statements, argumentGraph.arguments)];
		edges = toEdges(argumentGraph.statements, argumentGraph.arguments);
	}


	private function toVertices(statements: Statement[], arguments: Argument[]): Vertex[] {
		var statementBoxes: StatementBox[];
		var argumentBoxes: ArgumentBox[];
		// 1. create bound vertices
		// for statements
		for (s in statements) {	
			if (argumentGraph.broughtForth(s) or argumentGraph.isPremise(s)) {
				var statementBox: StatementBox = StatementBox {
					statement: bind s
					level: bind { statementBox.parentVertex.level + 1}
					control: bind control
				}
				insert statementBox into statementBoxes;
			}
		}
		// for arguments
		for (a in arguments) {
				var argumentBox: ArgumentBox = ArgumentBox {
				argument: bind a
				level: bind { argumentBox.parentVertex.level + 1}
				control: bind control
			}
			insert argumentBox into argumentBoxes;
		}
	
		// 2. determine parent/child and set it
		// for statements
		for (s in statementBoxes) {
			var found: Vertex[];
			for (a in argumentBoxes){
				for (p in a.argument.premises) {
					if (p.statement == s.statement) {
						insert a into found;
						insert s into a.children
					}
				}
			}
			if (sizeof found > 0) {
				s.parentVertex = found [0];
			} else if (s != root) {
				s.parentVertex = root;
				insert s into root.children;
			}
		}
		// for arguments
		for (a in argumentBoxes) {
			var found: Vertex[];
			for (s in statementBoxes) {
				if (a.argument.conclusion == s.statement) {
					insert s into found;
					insert a into s.children;
				}	
			}
			if (sizeof found > 0) {
				a.parentVertex = found [0];
			} else if (a != root) {
				a.parentVertex = root;
				insert a into root.children;
			}
		}
		//GC.p("Done updating nodes.");
		//GC.p("# rootchildren: " + sizeof root.children);
		return [argumentBoxes, statementBoxes];
	}
	
	private function toEdges(statements: Statement[], arguments: Argument[]): Edge[] {
		var links: Edge[];

		// 1. argument links
		for (a in arguments where (a.conclusion != null)) {
			var link: Edge;
			var producer: ArgumentBox[] = (for (v in vertices where 
											((v instanceof ArgumentBox) 
											and (v as ArgumentBox).argument == a)) 
												{ v as ArgumentBox});
			var recipient: StatementBox[] = (for (v in vertices where 
											((v instanceof StatementBox) 
											and (v as StatementBox).statement == a.conclusion)) 
												{ v as StatementBox });
			if (a.pro) {
			link = ProArgumentLink {
				producer: producer[0]
				recipient: recipient[0]
				control: bind control
				}
			} else
			link = ConArgumentLink {
				producer: producer[0]
				recipient: recipient[0]
				control: bind control
			}
			insert link into links;
		}

		// 2. Premises
		for (a in arguments) {
			for (p in a.premises) {
				var link: Edge;
				var recipient: ArgumentBox[] = (for (v in vertices where 
												((v instanceof ArgumentBox) 
												and (v as ArgumentBox).argument == a)) 
													{ v as ArgumentBox});
				var producer: StatementBox[] = (for (v in vertices where 
												((v instanceof StatementBox) 
												and (v as StatementBox).statement == p.statement)) 
													{ v as StatementBox });
				link = PremiseLink {
					premise: bind p
					producer: producer[0]
					recipient: recipient[0]
					negated: bind p.negative
					control: bind control
				}	
				insert link into links;
			}
		}
		return links;
	}


	// OTHER FUNCTIONS

}
