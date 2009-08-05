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


import javafx.animation.KeyFrame;
import javafx.animation.Timeline;

/**
 * The base class for view graphs.
 */
public class Graph extends CustomNode {

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
	* The group of elements that are fading out.
	*/
	public var fadingElements: GraphElement[] = [];

	/**
	* Function to remove elements from the displayed graph to give them the possibility to be faded out.
	*/
	public function removeWithFade(e: GraphElement): Void {
	    if (e instanceof Vertex) {
		e.toBeHidden = true;
		// mg: this causes an error -> hence no fade out
		//insert e into fadingElements;
		delete e as Vertex from vertices;
	    }
	    if (e instanceof Edge) {
		e.toBeHidden = true;
		// mg: this causes an error -> hence no fade out
		//insert e into fadingElements;
		delete e as Edge from edges;
	    }
	}

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
		if (e.selected) {
		    insert e.model into selectedModels;
		}
	    }
	}

	/**
	 * The layout which is used to display the graph.
	 */
	public var glayout: GraphLayout;

	/**
	 * The function which is called to create a view graph from a model graph.
	 */
	public function updateFromModel() {}

	public function updateDisplay() {
	    glayout.compose();
	    applyNewLayout();
	}

	public function applyNewLayout() {
	    controlsLocked = true;

	    // make all invisible vertices visible
	    for (v in vertices where v.toBeDisplayed and not v.visible) {
		v.visible = true;
	    }
	    for (v in vertices where v.toBeDisplayed) {
		v.x = v.xNew;
		v.y = v.yNew;
	    }
	    for (v in vertices) {
		v.xOld = v.x;
		v.yOld = v.y;
	    }

	    // make all invisible edges visible
	    for (e in edges where e.toBeDisplayed and not e.visible) {
		e.visible = true;
	    }
	    for (e in edges where e.toBeDisplayed) {
		e.x1 = e.x1New;
		e.y1 = e.y1New;
		e.x2 = e.x2New;
		e.y2 = e.y2New;
	    }
	    for (e in edges) {
		e.x1Old = e.x1;
		e.y1Old = e.y1;
		e.x2Old = e.x2;
		e.y2Old = e.y2;
	    }

	    Timeline {
		repeatCount: 1
		keyFrames: [
		    KeyFrame {
			time: 0s
			values: [
			    // for all vertices that are there and are supposed to stay there
			    for (v in vertices where not v.toBeDisplayed ) {
				v.x => v.xOld;
				v.y => v.yOld;
			    },
			    // for all vertices that are not there and are supposed to appear
			    for (v in vertices where v.toBeDisplayed) {
				v.opacity => 0.0;
			    }
			    // for all vertices that are there and are supposed to disappear
			    for (v in vertices where v.toBeHidden) {
				v.opacity => 1.0;
			    }

			    // for all edges that are there and are supposed to stay there
			    for (e in edges where not e.toBeDisplayed ) {
				e.x1 => e.x1Old;
				e.y1 => e.y1Old;
				e.x2 => e.x2Old;
				e.y2 => e.y2Old;
			    },
			    // for all edges that are not there and are supposed to appear
			    for (e in edges where e.toBeDisplayed) {
				e.opacity => 0.0;
			    }
			    // for all edges that are there and are supposed to disappear
			    for (e in edges where e.toBeHidden) {
				e.opacity => 1.0;
			    }
			]
		    },
		    KeyFrame {
			time: 0.5s
			action: function(): Void {
			    // for all vertices that are there and are supposed to stay there
			    for (v in vertices where not v.toBeDisplayed) {
				v.x = v.xNew;
				v.y = v.yNew;
				v.xOld = v.x;
				v.yOld = v.y;
			    }

			    // for all edges that are there and are supposed to stay there
			    for (e in edges where not e.toBeDisplayed) {
				e.x1 = e.x1New;
				e.y1 = e.y1New;
				e.x1Old = e.x1;
				e.y1Old = e.y1;
				e.x2 = e.x2New;
				e.y2 = e.y2New;
				e.x2Old = e.x2;
				e.y2Old = e.y2;
			    }
			}
			values: [
			    // for all vertices that are there and are supposed to stay there
			    for (v in vertices where not v.toBeDisplayed) {
				[v.x => v.xNew,
				v.y => v.yNew]
			    },
			    // for all vertices that are not there and are supposed to appear
			    for (v in vertices where v.toBeDisplayed) {
				v.opacity => 1.0;
			    }
			    // for all vertices that are there and are supposed to disappear
			    for (v in vertices where v.toBeHidden) {
				v.opacity => 0.0;
			    }

			    // for all edges that are there and are supposed to stay there
			    for (e in edges where not e.toBeDisplayed) {
				[
				    e.x1 => e.x1New,
				    e.y1 => e.y1New,
				    e.x2 => e.x2New,
				    e.y2 => e.y2New
				]
			    },
			    // for all edges that are not there and are supposed to appear
			    for (e in edges where e.toBeDisplayed) {
				e.opacity => 1.0;
			    }
			    // for all edges that are there and are supposed to disappear
			    for (e in edges where e.toBeHidden) {
				e.opacity => 0.0;
			    }
			]
		    },
		    KeyFrame {
			time: 0.51s
			action: function(): Void {
			    // for all vertices that are not there and are supposed to appear
			    for (v in vertices where v.toBeDisplayed) {
				v.toBeDisplayed = false;
			    }
			    // for all edges that are not there and are supposed to appear
			    for (e in edges where e.toBeDisplayed) {
				e.toBeDisplayed = false;
			    }
			    // for all vertices that are there and are supposed to disappear
			    for (v in vertices where v.toBeHidden) {
				v.toBeHidden = false;
				delete v from fadingElements;
			    }
			    // for all edges that are not there and are supposed to appear
			    for (e in edges where e.toBeHidden) {
				e.toBeHidden = false;
				delete e from fadingElements;
			    }
			    controlsLocked = false;
			}
		    }
		]
	    }.play();
	}

	override function create():Node {
		Group {
			content: bind [
				vertices,
				edges,
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

	public function print() {
		for (v in vertices) {
			v.print();
		}
	}

} // Graph
