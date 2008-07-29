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

import javafx.ext.swing.*;
import javafx.input.*;
import javafx.animation.*;
import javafx.scene.geometry.*;
import javafx.scene.paint.*;
import javafx.scene.*;
import Carneades.Graph.*;
import java.lang.System;

// Abstract Controller Class for Interaction
import Carneades.Control.GraphControl;

public class GraphView extends CustomNode {
	public attribute focusX: Number = middleX;
	public attribute focusY: Number = middleY;
	public attribute middleX: Number = bind this.width/2 on replace { graph.translateX = middleX };
	public attribute middleY: Number = bind this.height/2 on replace { graph.translateY = middleY };
	private attribute tempX: Number = 0;
	private attribute tempY: Number = 0;
	
	public attribute graph: Graph;
	public attribute width: Number;
	public attribute height: Number;
	public attribute layout: GraphLayout;
	public attribute control: GraphControl;

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
		fill: GC.viewBackground
		visible: true
	}

	// sensor object to detect mouse events outside of nodes.
	attribute backSensor: Rectangle = Rectangle {
		x: 0
		y: 0
		height: bind this.height
		width: bind this.width
		fill: GC.transparent
		visible: true

		onMouseClicked: function(e: MouseEvent): Void {
			// unselect if right mouse button pressed
			if (e.getButton() == 3) {
				control.unSelectAll();
			}
		}

		onMousePressed: function(e: MouseEvent): Void {
			// If we are dragging the view, backup the press location.
			if (not control.dragging and e.getButton() == 1 and e.isShiftDown()) {
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
			if (control.dragging and e.getButton() == 1) {
				dragSymbol.x = e.getStageX() - 10;
				dragSymbol.y = e.getStageY() - 6;
			// If we are dragging the view, set the new focus to the 
			// respective coordinates relative to the backup values.
			} else if (not control.dragging and e.getButton() == 1 and e.isShiftDown()) {
				graph.root.xShift = tempX + e.getDragX();
				graph.root.yShift = tempY + e.getDragY();

				// do bounds check - the vertical check only works for the treelayout as of now.
				if (graph.root.xShift > (layout.width / 2)) 
					{ graph.root.xShift = (layout.width / 2)}
				if (graph.root.xShift < (-layout.width / 2)) 
					{ graph.root.xShift = (-layout.width / 2)}
				if (graph.root.yShift + layout.height < - this.height / 2) 
					{ graph.root.yShift = - layout.height - this.height / 2}
				if (graph.root.yShift + GC.yDistance + GC.vertexDefaultHeight > (this.height / 2)) 
					{ graph.root.yShift = (this.height / 2) + GC.yDistance + GC.vertexDefaultHeight}
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
		fill: GC.dragColor
		visible: bind control.dragging
	}

	attribute middlePoint = Circle {
		centerX: bind middleX
		centerY: bind middleY
		radius: 5
		fill: Color.BLUE
		visible: bind GC.debug
	}

	public function create(): Node {
		return Group {
			content: bind [ background, graph, backSensor, dragSymbol, middlePoint ]
		}
	}

	public function reset(): Void {
		graph.translateX = middleX;
		graph.translateY = middleY;
	}

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
							time: (GC.zoomTime) * 0.5s
							values: [
									 zoomFactor => (0.7 * oldZoom) tween Interpolator.EASEBOTH,
							]
						},
						KeyFrame {
							time: (GC.zoomTime) * 1.0s
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
}
