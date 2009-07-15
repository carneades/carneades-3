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


package carneadesgui.view;

import javafx.scene.*;
import carneadesgui.view.Vertex;
import carneadesgui.GC.*;
import carneadesgui.view.GraphLayout;
import java.lang.Object;

import carneadesgui.control.CarneadesControl;

/**
 * The base class for view graphs.
 */
public class Graph extends CustomNode {

	/**
	 * The corresponding control object of the application.
	 */
	public var control: CarneadesControl;

	/**
	 * The sequence of vertices that make up the graph.
	 */
	public var vertices: Vertex[];

	/**
	 * The graph's root vertex. Note that this usually is an invisible node and does not correspond to a model object.
	 */
	public var root: Vertex;

	/**
	 * The edges connecting the vertices.
	 */
	public var edges: Edge[];

	/**
	 * The sequence of selected model objects.
	 */
	public var selectedModels: Object[] = [];

	/**
	* Returns the list of selected GraphElements
	*/
	public function selectedElements(): GraphElement[] {
		return [for (e in [vertices, edges] where (e as GraphElement).selected == true) {e}]
	}

	/**
	* Updates the graph's selected element list from the selectedModels list.
	*/
	public function updateSelectedElementsFromModel(): Void {
		for (m in selectedModels) {
			for (e in [vertices, edges] where (e as GraphElement).model == m) { e.selected = true; }
		}
	}

	/**
	* Updates the graph's selected model list from the selected elements.
	*/
	public function updateSelectedModelsFromElements(): Void {
		var s: GraphElement[] = selectedElements();
		for (e in s) {
			if (e.selected) insert e.model into selectedModels;
		}
	}

	/**
	 * The layout which is used to display the graph.
	 */
	public var glayout: GraphLayout;

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
		selectedModels = [];
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
