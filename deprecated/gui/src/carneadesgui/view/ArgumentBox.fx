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

// import the necessary parts of the model
import carneadesgui.model.Argument;
import carneadesgui.model.Argument.*;

// Other View Imports
import carneadesgui.view.*;
import carneadesgui.GC.*;

// control import
import javafx.scene.Node;
import javafx.scene.input.MouseEvent;
import javafx.scene.shape.Circle;
import javafx.scene.transform.Translate;
import javafx.scene.input.MouseButton;
import javafx.scene.paint.Color;
import javafx.scene.shape.Circle;
import javafx.scene.text.TextAlignment;
import javafx.scene.text.TextOrigin;

import carneadesgui.model.Argument.BeyondReasonableDoubt;
import carneadesgui.model.Argument.ClearAndConvincingEvidence;
import carneadesgui.model.Argument.Preponderance;
import carneadesgui.view.Elements.ArgumentElement;
import carneadesgui.view.Elements.CenteredStatementText;

import carneadesgui.control.XWDocumentBuilder.XWText;
import carneadesgui.control.XWDocumentBuilder.XWElement;
import carneadesgui.control.XWDocumentBuilder.XWDocument;
import carneadesgui.control.XWDocumentBuilder.XWAttribute;

/**
 * The argument view object. Currently it is a circle, but it used to be a box, hence the name.
 */
public class ArgumentBox extends ArgumentElement {

    /**
     * The represented model argument object.
     */
    public var argument: Argument;

    // bind the model element to the argument element
    override var model = bind argument;

	override def level = bind { parentVertex.level + 1}
    override var cache = true;
    override var height = argumentCircleDefaultRadius * 2;
    override var width = argumentCircleDefaultRadius * 2;
    override def caption = bind argument.id;
    override def fill = bind {if (argument.ok) Color.LIGHTGREY else Color.WHITE};
    override var bottomBrink = argumentBoxBottomBrink;

    def text = CenteredStatementText {
		content: bind {
			if ((argument.conclusion.standard) instanceof Preponderance
				or (argument.conclusion.standard) instanceof ClearAndConvincingEvidence
				or (argument.conclusion.standard) instanceof BeyondReasonableDoubt
				)
				"{if (argument.pro) '+' else '-'}.{(argument.weight * 100) as Integer}"
			else "{if (argument.pro) '+' else '-'}"
		}
		textAlignment: TextAlignment.CENTER
		textOrigin: TextOrigin.BASELINE
		x: bind x
		// The text Y coordinate positioning is dirty as the text currently lacks a currentheight attribute
		y: bind y + 10
	} // Text

	var mainCircle: Circle = Circle {
		centerX: bind x
		centerY: bind y
		radius: bind argumentCircleDefaultRadius

		fill: bind {
			if (not argument.ok) Color.WHITE
			else {
				if (not argument.pro) argumentConColor else argumentProColor
			}
		}

		stroke: bind { if (mainCircle.hover) Color.GREY else Color.BLACK }
		blocksMouse: false

		onMousePressed: function(e: MouseEvent): Void {
			control.processGraphSelection(this);
		}

		onMouseDragged: function(e: MouseEvent) {
			if (this.selected and (e.button == MouseButton.PRIMARY)) { control.startDrag(this); }
		}

		onMouseReleased: function(e: MouseEvent) {
			if (control.dragging) {	control.endDrag(e.shiftDown); }
		}

		onMouseEntered: function(e: MouseEvent) {
			if (control.dragging) { control.setDraggingOver(this); }
		}

		onMouseExited: function(e: MouseEvent) {
			if (control.dragging) { control.setDraggingOver(null); }
		}
	}

    override var selection = Circle {
		fill: Color.TRANSPARENT
		centerX: bind x
		centerY: bind y
		radius: argumentCircleDefaultRadius + 5
		stroke: bind {if (control.dragging) dragColor else selectionColor};
		strokeWidth: 2
		visible: bind selected
    } // selection circle


    override function create():Node {
		Group {
			content: [
				mainCircle,
				selection,
				text,
				// middlePoint
			] // content
		} // Group
    } // composeNode

	override function toSVG(d: XWDocument): XWElement {
		XWElement {
			name: "g"
			document: d
			attributes: [
				XWAttribute {
					name: "transform"
					value: "translate({graph.boundsInLocal.width / 2 + SVG_LEFTOFFSET}, 0)"
				},
			]
			children: [
				XWElement {
					name: "circle"
					document: d
					attributes: [
						XWAttribute {
							name: "cx"
							value: "{x}"
						},
						XWAttribute {
							name: "cy"
							value: "{y}"
						},
						XWAttribute {
							name: "r"
							value: "{argumentCircleDefaultRadius}"
						},
						XWAttribute {
							name: "fill"
							value: "{toSVGColorCode(mainCircle.fill)}"
						},
						XWAttribute {
							name: "opacity"
							value: "{mainCircle.opacity}"
						},
						XWAttribute {
							name: "stroke"
							value: "{toSVGColorCode(mainCircle.stroke)}"
						},
						XWAttribute {
							name: "stroke-width"
							value: "{mainCircle.strokeWidth}"
						},
					]
				},
				XWElement {
					document: d
					name: "text"
					attributes: [
						XWAttribute {
							name: "x"
							value: "{text.x}"
						},
						XWAttribute {
							name: "y"
							value: "{text.y + (text.transforms[0] as Translate).y}"
						},
						XWAttribute {
							name: "text-anchor"
							value: "middle"
						},
						XWAttribute {
							name: "width"
							value: "{width}"
						},
						XWAttribute {
							name: "height"
							value: "{height}"
						},
						XWAttribute {
							name: "fill"
							value: "{toSVGColorCode(Color.BLACK)}"
						},
						XWAttribute {
							name: "font-size"
							value: "{STATEMENTBOX_FONTSIZE}"
						},
						XWAttribute {
							name: "font-family"
							value: "monospace"
						},
					]
					children: [
						XWText {
							value: "{text.content}"
						}
					]
				}
			]
		}
	}
}
