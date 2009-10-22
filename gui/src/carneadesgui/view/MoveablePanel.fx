/*
Carneades Argumentation Library and Tools.
Copyright (C) 2008 Thomas Gordon and Matthias Grabmair

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

import carneadesgui.GC.*;

import javafx.scene.CustomNode;
import javafx.scene.Node;
import javafx.scene.paint.Color;
import javafx.geometry.HPos;
import javafx.scene.layout.Stack;
import javafx.geometry.VPos;
import javafx.scene.shape.Rectangle;
import javafx.scene.layout.VBox;
import javafx.scene.text.Text;
import javafx.scene.input.MouseButton;
import javafx.scene.input.MouseEvent;


import javafx.scene.image.Image;


public class MoveablePanel extends CustomNode {
	public var positionLocked: Boolean = false;
	public var title: String = "Window Title";
	public var display: Boolean = true;
	public var width: Number;
	public var height: Number;
	public var x: Number;
	public var y: Number;
	public var content: Node[];
	public var padding: Number;

	public var onClose: function(): Void;

	var dragX: Number;
	var dragY: Number;
	public var maxX: Number;
	public var minX: Number;
	public var maxY: Number;
	public var minY: Number;

	override def translateX = bind {
		if (x + dragX < minX and dragging) minX
			else if (x + dragX > maxX and dragging) maxX
				else x + dragX};
	override def translateY = bind {
		if (y + dragY < minY and dragging) minY
			else if (y + dragY > maxY and dragging) maxY
				else y + dragY};
	var dragging: Boolean = false;

	def titleText: Text = Text {
		translateX: padding + 3
		translateY: padding + 3
		content: bind title
		blocksMouse: false
		fill: Color.WHITE
	}

	def titleRectangle: Rectangle = Rectangle {
		translateX: padding
		translateY: padding
		width: this.width - 2 * padding + 2
		height: MOVEABLEPANEL_TITLE_HEIGHT
		fill: MOVEABLEPANEL_TITLE_FILL
		stroke: null
		blocksMouse: true
		effect: MOVEABLEPANEL_TITLE_EFFECT
		onMouseDragged: function(e: MouseEvent) {
			if (not positionLocked)
				if (not controlsLocked) {
					if (e.button == MouseButton.PRIMARY) {
						if (not dragging) dragging = true;
						dragX = e.dragX;
						dragY = e.dragY;
					}
				}
		}


		onMouseReleased: function(e: MouseEvent) {
			if (not positionLocked)
				if (not controlsLocked) {
					if (e.button == MouseButton.PRIMARY) {
						if (x + dragX < minX) x = minX
							else if (x + dragX > maxX) x = maxX
								else x += dragX;
						if (y + dragY < minY) y = minY
							else if (y + dragY > maxY) y = maxY
								else y += dragY;
						dragX = 0;
						dragY = 0;
						dragging = false;
					}
				}
		}
	}

	def titleBar: Stack = Stack {
		nodeHPos: HPos.LEFT
		nodeVPos: VPos.TOP
		content: [
			titleRectangle,
			titleText,
			ImageButton {
				translateX: this.width - 2 * padding - MOVEABLEPANEL_TITLE_HEIGHT + 7
				translateY: padding
				width: MOVEABLEPANEL_TITLE_HEIGHT - 3
				height: MOVEABLEPANEL_TITLE_HEIGHT - 3
				image: Image { url: "{__DIR__}images/icon-stop.png"	}
				action: onClose
			}
		]
	}

	public function show(): Void {
		display = true;
		// The animation is commented out due to a glitch in the fade api
		/*FadeTransition {
			duration: 0.3s node: this
			fromValue: 0.0 toValue: 1.0
			repeatCount:1
		}.play(); */
	}

	public function hide(): Void {
		/*FadeTransition {
			duration: 0.3s node: this
			fromValue: 1.0 toValue: 0.0
			repeatCount:1
			action: function() {
				display = false;
			}
		}.play();*/
		display = false;
	}

	var mainArea: VBox = VBox {
		nodeHPos: HPos.LEFT
		spacing: padding
		content: bind [
			titleBar,
			PaddedBox {
				xPadding: padding
				yPadding: padding
				content: bind content
			}
		]
	}

	override function create(): Node {
		Stack {
			nodeHPos: HPos.LEFT
			nodeVPos: VPos.TOP
			content: bind {
				if (display) [
					LayoutRect {
						width: bind width
						height: bind height
						fill: panelBackground
						stroke: Color.BLACK
					}, mainArea
				] else null
			}
		} 
	}
}
