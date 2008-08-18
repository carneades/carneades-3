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

import javafx.scene.*;
import Carneades.Graph.Vertex;
import Carneades.Graph.Elements.Elements.*;
import java.lang.System;
import java.lang.Object;

import Carneades.Control.GraphControl;

/**
 * The base class for view graphs.
 */
public class Graph extends CustomNode {

	/**
	 * The corresponding control object of the application.
	 */
	public attribute control: GraphControl;

	/**
	 * The sequence of vertices that make up the graph.
	 */
	public attribute vertices: Vertex[];

	/**
	 * The graph's root vertex. Note that this usually is an invisible node and does not correspond to a model object.
	 */
	public attribute root: Vertex;

	/**
	 * The edges connecting the vertices.
	 */
	public attribute edges: Edge[];

	/**
	 * The sequence of selected model objects.
	 */
	public attribute selectedModels: Object[] = [];

	/**
	 * The sequence of selected view objects.
	 */
	public attribute selected: GraphElement[] = [];

	/**
	 * The layout which is used to display the graph.
	 */
	public attribute layout: GraphLayout;

	/**
	 * The function which is called to create a view graph from a model graph.
	 */
	public function update() {}

	override function create():Node {
		Group {
			content: bind [
				vertices, 
				edges
				]// bind
		} // Group
	} // composeNode

	/**
	 * Unselects all model and view objects.
	 */
	public function unSelectAll(): Void {
		for (i in vertices) i.selected = false;
		for (i in edges) i.selected = false;
		selected = [];
	}

	/**
	 * Print all vertex information to the console.
	 */
	public function print() {
		for (v in vertices) {
			v.print();;
		}
	}

} // Graph
