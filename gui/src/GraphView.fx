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
import javafx.ext.swing.*;
import javafx.input.*;
import javafx.animation.*;
import javafx.scene.geometry.*;
import javafx.scene.paint.*;
import javafx.scene.*;
import java.lang.System;

// view imports
import Carneades.Graph.*;
import Carneades.Graph.GC.*;
import Carneades.Graph.Elements.Elements.*;

// control imports
import Carneades.Control.GraphControl;

/**
 * The top node class for the scenegraph-based display of the graphs.
 */
public class GraphView extends CustomNode {

	/**
	 * The X coordinate of the point on which the view focuses during shifting and zooming.
	 */
	private attribute focusX: Number = middleX;

	/**
	 * The Y coordinate of the point on which the view focuses during shifting and zooming.
	 */
	private attribute focusY: Number = middleY;

	/**
	 * The canvas middle point X coordinate.
	 */
	public attribute middleX: Number = bind this.width/2 on replace { graph.translateX = middleX };

	/**
	 * The canvas middle point Y coordinate.
	 */
	public attribute middleY: Number = bind this.height/2 on replace { graph.translateY = middleY };

	private attribute tempX: Number = 0;
	private attribute tempY: Number = 0;
	
	/**
	 * The currently displayed view graph object.
	 */
	public attribute graph: Graph;

	/**
	 * The width of the view.
	 */
	public attribute width: Number;

	/**
	 * The height of the view.
	 */
	public attribute height: Number;

	/**
	 * The layout of the graph. Bound and read-only.
	 */
	public attribute layout: GraphLayout = bind graph.layout;

	/**
	 * The application's control object.
	 */
	public attribute control: GraphControl;

	/**
	 * The current zoom factor. It has a replace trigger on it that checks for bounds (current limits 0.1-2.0) and dynamically zooms the view by scaling the view graph.
	 */
	public attribute zoomFactor: Number = 1.0 on replace {
		if (zoomFactor < 0.1) { zoomFactor = 0.1 }
		if (zoomFactor > 2.0) { zoomFactor = 2.0 }
		graph.scaleX = zoomFactor;
		graph.scaleY = zoomFactor;
	};

	// background layer
	attribute background: Rectangle = Rectangle {
		x: 0
		y: 0
		height: bind this.height
		width: bind this.width
		fill: viewBackground
		visible: true
	}

	// sensor object to detect mouse events outside of nodes.
	attribute backSensor: Rectangle = Rectangle {
		x: 0
		y: 0
		height: bind this.height
		width: bind this.width
		fill: transparent
		visible: true

		onMouseClicked: function(e: MouseEvent): Void {
			// unselect if right mouse button pressed
			if (e.getButton() == 3) {
				control.unSelectAll();
			}
		}

		onMousePressed: function(e: MouseEvent): Void {
			// If we are dragging the view, backup the press location.
			if (not control.dragging and e.getButton() == 1 and (e.isShiftDown() or control.dragView) ) {
				tempX = graph.root.xShift;
				tempY = graph.root.yShift;
			}
		}

		onMouseReleased: function(e: MouseEvent): Void {
			// reset drag variables
			tempX = 0;
			tempY = 0;
		}

		onMouseDragged: function(e: MouseEvent): Void {
			if (control.dragging /*and e.getButton() == 1*/) {
				dragSymbol.x = e.getStageX() - 10;
				dragSymbol.y = e.getStageY() - 6;
			// If we are dragging the view, set the new focus to the 
			// respective coordinates relative to the backup values.
			} 
			if (not control.dragging /*and e.getButton() == 1*/ and (e.isShiftDown() or control.dragView)) {
				graph.root.xShift = tempX + e.getDragX();
				graph.root.yShift = tempY + e.getDragY();

				// do bounds check - the vertical check only works for the treelayout as of now.
				if (graph.root.xShift > (layout.width / 2)) 
					{ graph.root.xShift = (layout.width / 2)}
				if (graph.root.xShift < (-layout.width / 2)) 
					{ graph.root.xShift = (-layout.width / 2)}
				if (graph.root.yShift + layout.height < - this.height / 2) 
					{ graph.root.yShift = - layout.height - this.height / 2}
				if (graph.root.yShift + yDistance + vertexDefaultHeight > (this.height / 2)) 
					{ graph.root.yShift = (this.height / 2) + yDistance + vertexDefaultHeight}
			}
		}

		onMouseWheelMoved: function(e: MouseEvent): Void {
			zoomFactor -= (e.getWheelRotation() / 20);
		}
	}

	attribute dragSymbol = Rectangle {
		x: 0
		y: 0
		width: 20
		height: 12
		fill: dragColor
		visible: bind control.dragging
	}

	attribute middlePoint = Circle {
		centerX: bind middleX
		centerY: bind middleY
		radius: 5
		fill: Color.BLUE
		visible: bind debug
	}

	override function create(): Node {
		return Group {
			content: bind [ background, graph, backSensor, dragSymbol, middlePoint ]
		}
	}

	/**
	 * Zoom function for the mouse that takes a parameter in mouse wheel steps.
	 */
	public function zoom(steps: Number): Void {
		zoomFactor -= (steps / 20);
	}

	/**
	 * Recalculates the middle point of the canvas. Is called upon resizing the frame.
	 */
	public function reset(): Void {
		graph.translateX = middleX;
		graph.translateY = middleY - yDistance;
	}

	/**
	 * Shifts the focus to by a certain number of points on the X and Y axis, respectively. It does a google-earth type zooming shift to it. The used time can be influenced by setting the zoomTime constant in GC.fx.
	 */
	public function focusOn(x: Number, y: Number): Void {
		var oldXShift: Number = graph.root.xShift;
		var oldYShift: Number = graph.root.yShift;
		var oldZoom: Number = zoomFactor;
		focusX = x; 
		focusY = y;

		var t: Timeline = Timeline {
			repeatCount: 1
			keyFrames: [ 
						KeyFrame {
							time: 0s
							values: [
									 graph.root.xShift => oldXShift tween Interpolator.EASEBOTH,
									 graph.root.yShift => oldYShift tween Interpolator.EASEBOTH,
									 zoomFactor => oldZoom tween Interpolator.EASEBOTH,
							]
						},
						KeyFrame {
							time: (zoomTime) * 0.5s
							values: [
									 zoomFactor => (0.7 * oldZoom) tween Interpolator.EASEBOTH,
							]
						},
						KeyFrame {
							time: (zoomTime) * 1.0s
							values: [
									 graph.root.xShift => (oldXShift - focusX) tween Interpolator.EASEBOTH,
									 graph.root.yShift => (oldYShift - focusY) tween Interpolator.EASEBOTH,
									 zoomFactor => oldZoom tween Interpolator.EASEBOTH,
							]
						}
			]
		} // timeline
		t.start();
	}

	/**
	 * Returns true if a certain graph element is currently visible in the view area. The relevant factor here is the middle point of the element.
	 */
	public function isVisible(e: GraphElement): Boolean {
		var visible: Boolean = true;
		if (e instanceof StatementBox) {
			if ((e as Vertex).x > (this.width / zoomFactor) / 2) visible = false;
			if ((e as Vertex).x < -(this.width / zoomFactor) / 2) visible = false;
			if ((e as Vertex).y > (this.height / zoomFactor) / 2) visible = false;
			if ((e as Vertex).y < -(this.height / zoomFactor) / 2) visible = false;
		}
		return visible;
	}
}
