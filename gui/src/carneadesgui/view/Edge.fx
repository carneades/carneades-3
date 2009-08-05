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

// general imports
import javafx.scene.paint.*;

// view imports
import carneadesgui.view.*;

import javafx.scene.shape.Line;

// control imports

import javafx.scene.Group;
import javafx.scene.Node;

import javafx.scene.transform.Rotate;

/**
 * The base class for all edges in the view graphs
 */
public class Edge extends GraphElement {

	/**
	 * The vertex from which the directed edge is originating
	 */
	public var producer: Vertex;

	/**
	 * The Vertex to which the directed edge is going.
	 */
	public var recipient: Vertex; // ... and to which vertex it is going.

	// Horizontal origin of the edge.
	public var x1Old: Number = 0;
	public var x1: Number = 0;
	public var x1New: Number = 0;

	/**
	 * Vertical origin of the edge.
	 */
	public var y1Old: Number = 0;
	public var y1: Number = 0;
	public var y1New: Number = 0;

	/**
	* Absolute horizontal end point of the edge.
	*/
	public var x2Old: Number = 0;
	public var x2: Number = 0;
	public var x2New: Number = 0;

	/**
	 * Absolute vertical end point of the edge.
	 */
	public var y2Old: Number = 0;
	public var y2: Number = 0;
	public var y2New: Number = 0;

	/**
	* The Y shift of the head symbol.
	*/
	public var yHeadShift: Number = 5;

	/**
	* The stroke color of the edge.
	*/
	public var stroke: Color = Color.BLACK;

	/**
	* The stroke color of the edge.
	*/
	public var strokeWidth: Number = 1;

	// attributes for optional heads

	/**
	 * Is there an edge head that needs to be turned to a certain angle?
	 */
	public var turnHead: Boolean = false;

	// main line of the edge
	public var edgeLine: Line = Line {
	    startX: bind x1
	    startY: bind y1
	    endX: bind x2
	    endY: bind y2
	    stroke: bind stroke
	    strokeWidth: bind strokeWidth
	}

	/**
	 * Function computing the angle of the edge depending on origin and end point of the edge.
	 */
	protected bound function getHeadRotation(x1: Number, x2: Number, y1: Number, y2: Number): Rotate {
	    Rotate {
		pivotX: bind x2
		pivotY: bind y2
	    }
	}

	override function create():Node {
		Group {
			content: bind [
			    edgeLine,
			] // content
		} // Group
	} // composeNode

}
