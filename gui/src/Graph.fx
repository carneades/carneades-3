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


package GraphSketch1.Graph;

import javafx.gui.*;
import GraphSketch1.Graph.Vertex;
import GraphSketch1.Graph.Elements.Elements.*;
import java.lang.System;

import GraphSketch1.Control.AbstractGraphControl;

public class Graph extends CustomNode {
	attribute control: AbstractGraphControl;
	attribute vertices: Vertex[];
	attribute edges: Edge[];
	attribute selected: GraphElement[] = [];
	public function update(): Void {}

	public function create():Node {
		Group {
			content: bind [
				edges, vertices
				]// bind
		} // Group
	} // composeNode

	public function unSelectAll(): Void {
		for (i in vertices) i.selected = false;
		for (i in edges) i.selected = false;
	}

} // Graph
