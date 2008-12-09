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

// general imports
import javafx.input.*;
import javafx.scene.*;
import javafx.scene.paint.*;
import javafx.scene.geometry.*;
import java.lang.System;
import java.lang.Math;

// view imports
import Carneades.Graph.*;
import Carneades.Graph.GC.*;

// control imports
import Carneades.Control.GraphControl;

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

	/**
	 * Horizontal adjustment of the origin of the edge.
	 */
	public var x1Shift: Number = 0; 

	/**
	 * Horizontal adjustment of the end point of the edge.
	 */
	public var x2Shift: Number = 0; 

	/**
	 * Vertical adjustment of the origin of the edge.
	 */
	public var y1Shift: Number = 0; // Shift to where the edge LINE should stop

	/**
	 * Vertical adjustment of the end point of the edge.
	 */
	public var y2Shift: Number = 0;

	/**
	 * Additional vertical adjustment for the head of the edge depending on its shape.
	 */
	public var yHeadShift: Number = 0; // Additional Shift for the HEAD (Arrow, etc.) size to adjust additionally

	/**
	 * Additional horizontal adjustment for the head of the edge depending on its shape.
	 */
	public var xHeadShift: Number = 0;

	/**
	 * Horizontal origin of the edge.
	 */
	public var x1: Number = bind producer.x + x1Shift on replace { if (turnHead) setAngle() };

	/**
	 * Vertical origin of the edge.
	 */
	public var y1: Number = bind producer.y + y1Shift on replace { if (turnHead) setAngle() };

	/**
	 * Absolute horizontal end point of the edge. Is bound and read-only.
	 */
	public var x2: Number = bind recipient.x + x2Shift + xHeadShift on replace { if (turnHead) setAngle() };

	/**
	 * Absolute vertical end point of the edge. Is bound and read-only.
	 */
	public var y2: Number = bind recipient.y + y2Shift + yHeadShift on replace { if (turnHead) setAngle() };

	/**
	 * Direction from which the edge is hitting is recipient. Needed for edge head angle calculation purposes. By default, it is assumed that the edge hits the node on the bottom side.
	 */
	public var direction: Number = BOTTOM; 

	/**
	* The stroke color of the edge.
	*/
	public var stroke: Color = Color.BLACK;

	/**
	 * the stroke width of the edge.
	 */
	public var strokeWidth: Number = edgeStrokeWidth;

	/**
	 * Is the edge displayed in dashes.
	 */
	public var dashed: Boolean = false;

	/**
	 * The application's control object.
	 */
	public var control: GraphControl;

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
	var edgeLine: Line = Line {
					startX: bind x1
					startY: bind y1
					endX: bind x2
					endY: bind y2
					stroke: bind stroke
					strokeWidth: bind strokeWidth
					strokeDashArray: bind { if (dashed) [5.0, 5.0] else [1.0] }
					strokeDashOffset: bind { if (dashed) 0.0 else 0.0 }
				}
	
	// invisible wider line for easier selection
	var selectLine = Line {
					startX: bind x1
					startY: bind y1
					endX: bind x2
					endY: bind y2
					stroke: bind transparent
					strokeWidth: bind strokeWidth + edgeSelectionWidth;

					onMouseClicked: function(e: MouseEvent) {
						control.unSelectAll();
						selected = true;
						control.processSelection();
					}
				}

	// newly colored line once selected
	var selection: Line = Line {
					startX: bind x1
					startY: bind y1
					endX: bind x2
					endY: bind y2
					stroke: bind selectionColor
					strokeWidth: bind selectedEdgeWidth;
					visible: bind selected
					strokeDashArray: bind { if (dashed) [5.0, 5.0] else [1.0] }
					strokeDashOffset: bind { if (dashed) 0.0 else 0.0 }
				}

	var line: Group = Group {
		content: [
			edgeLine, selection, selectLine
		]
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
			content: [
				line,	
			] // content
		} // Group
	} // composeNode

}
