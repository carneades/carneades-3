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
import Carneades.Graph.*;
import java.lang.System;

// Abstract Controller Class for Interaction
import Carneades.Control.GraphControl;

public class GraphView extends Canvas {
	public attribute focusX: Number = middleX;
	public attribute focusY: Number = middleY;
	public attribute middleX: Number = bind this.width/2;
	public attribute middleY: Number = bind this.height/3;
	private attribute tempX: Number = 0;
	private attribute tempY: Number = 0;
	
	public attribute graph: Graph;
	override attribute width;
	attribute layout: GraphLayout;
	attribute control: GraphControl;

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
				dragSymbol.x = e.getCanvasX() - 10;
				dragSymbol.y = e.getCanvasY() - 6;
			// If we are dragging the view, set the new focus to the 
			// respective coordinates relative to the backup values.
			} else if (not control.dragging and e.getButton() == 1 and e.isShiftDown()) {
				graph.root.xShift = tempX + e.getDragX();
				graph.root.yShift = tempY + e.getDragY();
				// do bounds check
				if (graph.root.xShift > (layout.width / 2) + middleX) { graph.root.xShift = (layout.width / 2) + middleX}
				if (graph.root.xShift < (-layout.width / 2) + middleX) { graph.root.xShift = (-layout.width / 2) + middleX}
				if (graph.root.yShift > (layout.height / 2) + middleY) { graph.root.yShift = (layout.height / 2) + middleY}
				if (graph.root.yShift < (-layout.height / 2) + middleY) { graph.root.yShift = (-layout.height / 2) + middleY}
			}
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

	override attribute background = GC.viewBackground;
	override attribute content = bind [graph, backSensor, dragSymbol ];

	public function reset(): Void {
		graph.root.xShift = middleX;
		graph.root.yShift = middleY - GC.yDistance - GC.vertexDefaultHeight;
	}

	public function focusOn(x: Number, y: Number): Void {
		focusX = x - graph.root.xShift;
		focusY = y - graph.root.yShift;

		var t: Timeline = Timeline {
			repeatCount: 1
			keyFrames: [ 
						KeyFrame {
							time: 0s
							values: [
									 graph.root.xShift => graph.root.xShift tween Interpolator.EASEBOTH,
									 graph.root.yShift => graph.root.yShift tween Interpolator.EASEBOTH
							]
						},
						KeyFrame {
							time: 0.5s
							values: [
									 graph.root.xShift => (middleX - focusX) tween Interpolator.EASEBOTH,
									 graph.root.yShift => (middleY - focusY) tween Interpolator.EASEBOTH
							]
						}
			]
		} // timeline
		t.start();
	}
}
