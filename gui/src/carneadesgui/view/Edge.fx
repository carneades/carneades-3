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
import java.lang.Math;

// view imports
import carneadesgui.view.*;
import carneadesgui.GC.*;

import javafx.scene.input.MouseEvent;
import javafx.scene.shape.Line;

// control imports
import carneadesgui.control.CarneadesControl;

import javafx.scene.Group;
import javafx.scene.Node;

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
	* The X shift of the head symbol.
	*/
	public var xHeadShift: Number = 0;

	/**
	* The Y shift of the head symbol.
	*/
	public var yHeadShift: Number = 0;

	/**
	* The stroke color of the edge.
	*/
	public var stroke: Color = Color.BLACK;

	/**
	* The stroke color of the edge.
	*/
	public var strokeWidth: Number = 1;

	/**
	 * Is the edge displayed in dashes.
	 */
	public var dashed: Boolean = false;

	// attributes for optional heads

	/**
	 * Is there an edge head that needs to be turned to a certain angle?
	 */
	public var turnHead: Boolean = false;

	/**
	 * Variable to store the computed angle head.
	 */
	public var angle: Number = 0;

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
	protected function setAngle():Void {
		// determine arrowhead angle
		// This code is highly redundant and suboptimal, but it was the only way which worked for "Transform.rotate"
		angle = 0;

		if (x1 == x2) {
			if (y2 > y1) { // vertical arrow
				angle = 90; // arrow top down
			} else {
				angle = -90; // arrow bottom up
			}
		} else
		if (y1 == y2) { // horizontal arrow
			if (x2 > x1) { // arrow pointing right
				angle = 0;
			} else { // arrow pointing left
				angle = 180;
			}
		} else // tilted arrow
		if (x1 < x2) {
			// arrow pointing right
			angle = ((Math.atan((Math.abs(y2-y1))/(x2-x1)) / (Math.PI/2)) * 90);
			if (y2 < y1) angle *= -1;
		} else {
			angle = ((Math.atan((Math.abs(y2-y1))/(x2-x1)) / (Math.PI/2)) * 90 + 180);
			if (y2 < y1) angle *= -1;
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
