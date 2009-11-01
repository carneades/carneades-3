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

import javafx.scene.paint.*;
import javafx.scene.text.*;
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
import javafx.scene.paint.Color;
import javafx.scene.shape.Circle;
import javafx.scene.shape.Rectangle;
import javafx.scene.shape.Line;
import javafx.scene.shape.Polygon;
import javafx.scene.shape.Shape;
import javafx.scene.transform.Translate;
import javafx.scene.text.TextAlignment;
import javafx.scene.text.TextOrigin;
import javafx.scene.input.MouseButton;
import javafx.scene.transform.Rotate;
import javafx.scene.image.Image;
import javafx.scene.image.ImageView;

import carneadesgui.control.XWDocumentBuilder.XWText;
import carneadesgui.control.XWDocumentBuilder.XWElement;
import carneadesgui.control.XWDocumentBuilder.XWDocument;
import carneadesgui.control.XWDocumentBuilder.XWAttribute;

/**
* Auxiliary class for centered text.
*/
public class CenteredStatementText extends Text {

	public var boundingHeight: Number;
    override var textAlignment = TextAlignment.CENTER;
    override var textOrigin = TextOrigin.TOP;
	override var blocksMouse = false;

    override var transforms = bind [
		Translate {
			x: - boundsInLocal.width / 2 /* hand correction: */ + 0
			y: - boundsInLocal.height / 2 /* hand correction: */ + 2
		}
    ];

	override var font = Font {
		name: "Monaco"
		size: STATEMENTBOX_FONTSIZE
	}

	public function changeText(t: String): String {
		content = toEscapeLines(t);
	}
}

/**
 * Base class for model-specific graph vertices.
 */
public abstract class ArgumentElement extends CarneadesVertex {

    /**
     * The filler color.
     */
    public var fill: Color = Color.WHITE;

    protected var selection: Shape = Rectangle {
		blocksMouse: false
		fill: Color.TRANSPARENT
		x: bind x - (width / 2) - 5
		y: bind y - (height / 2) - 5
		width: bind width + 13
		height: bind height + 10
		stroke: bind {if (control.dragging) dragColor else selectionColor};
		strokeWidth: 2
		visible: bind selected
    } // selection rect

    def middlePoint: Circle = Circle {
		centerX: bind x
		centerY: bind y
		radius: 3
		fill: Color.RED
    }
}

/**
 * Class for edges that have arrow heads.
 */
public class Arrow extends CarneadesEdge {

    /**
     * The size of the arrowhead in side length pixels.
     */
    public var headSize: Number = 10;

    /**
     * The color to fill the arrowhead with.
     */
    public var fill: Color = Color.BLACK;

    override function create():Node {
		Group {
			content: [
				edgeLine,
				Polygon {
					transforms: bind getHeadRotation(x1, x2, y1, y2)
					points: bind [ x2, y2,
					x2 - (headSize / 2), y2 + headSize,
					x2 + (headSize / 2), y2 + headSize]
					stroke: bind stroke
					strokeWidth: bind strokeWidth
					fill: bind fill
				}
			] // content
		} // group
    }
}

/**
 * Argument link specific arrow class to set base attributes.
 */
public class ArgumentLink extends Arrow {
    override var headSize = 10;
    override var yHeadShift = bind headSize / 3;

    public var argument: Argument = null;
    // bind the model element to the argument element
    override var model = bind argument;

    override var fill = bind {if (argument.pro) Color.BLACK else Color.WHITE};

	override function toSVG(d: XWDocument) {
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
					name: "line"
					document: d
					attributes: [
						XWAttribute {
							name: "x1"
							value: "{x1}"
						},
						XWAttribute {
							name: "y1"
							value: "{y1}"
						},
						XWAttribute {
							name: "x2"
							value: "{x2}"
						},
						XWAttribute {
							name: "y2"
							value: "{y2}"
						},
						XWAttribute {
							name: "stroke-width"
							value: "{strokeWidth}"
						},
						XWAttribute {
							name: "stroke"
							value: "{toSVGColorCode(stroke)}"
						}
					]
				},
				XWElement {
					name: "polygon"
					document: d
					attributes: [
						XWAttribute {
							name: "fill"
							value: "{toSVGColorCode(fill)}"
						},
						XWAttribute {
							name: "points"
							value: "{x2}, {y2} {x2 - (headSize / 2)}, {y2 + headSize} {x2 + (headSize / 2)}, {y2 + headSize}"
						},
						XWAttribute {
							name: "stroke"
							value: "{toSVGColorCode(stroke)}"
						},
						XWAttribute {
							name: "stroke-width"
							value: "{strokeWidth}"
						},
						XWAttribute {
							name: "transform"
							value: "rotate({- (java.lang.Math.atan((x2-x1)/(y2-y1)) / java.lang.Math.PI) * 180} {x2} {y2})"
						},
					]
				}
			]
		}
	}
}

/**
 * Class for premise link edges.
 */
public class PremiseLink extends CarneadesEdge {

    /**
     * The represented model premise object.
     */
    public var premise: Premise;

    // bind the model element to the premise element
    override def model = bind premise;

    /**
     * The radius of the head at the edges end. Deprecated! Was used in older design of assumptions and exceptions.
     */
    public var radius: Number = 5;

    /**
     * The width of the negation bar.
     */
    public var negWidth: Number = 10;

    /**
     * The stroke width of the negation bar.
     */
    var negStrokeWidth: Number = 2;

    /**
     * Shall the negation bar be drawn?
     */
    public var negated: Boolean = false;

	override var dashArray = bind { if (premise.exception) [6.0, 6.0] else [1.0] };

    def action = function(e: MouseEvent): Void { control.processGraphSelection(this) }

    // this is supposed to be a line but the line will not give mouse click events
    function getLengthOfEdge(x1: Number, x2: Number, y1: Number, y2: Number) {
		java.lang.Math.sqrt((x1-x2)*(x1-x2) + (y1-y2)*(y1-y2))
    }

    var selectPoly: Polygon = Polygon {
		points: bind [
			(x1+x2)/2 - 3, (y1+y2)/2 + getLengthOfEdge(x1, x2, y1, y2) / 2,
			(x1+x2)/2 + 3, (y1+y2)/2 + getLengthOfEdge(x1, x2, y1, y2) / 2,
			(x1+x2)/2 + 3, (y1+y2)/2 - getLengthOfEdge(x1, x2, y1, y2) / 2,
			(x1+x2)/2 - 3, (y1+y2)/2 - getLengthOfEdge(x1, x2, y1, y2) / 2,]
		transforms: bind Rotate {
			pivotX: bind (x2 + x1) / 2
			pivotY: bind (y2 + y1) / 2
			angle: - (java.lang.Math.atan((x2-x1)/(y2-y1)) / java.lang.Math.PI) * 180
		}
		blocksMouse: true
		fill: Color.GREEN
		opacity: 0.0
		onMouseClicked: action
    }

    // newly colored line once selected
    var selection: Line = Line {
		startX: bind x1
		startY: bind y1
		endX: bind x2
		endY: bind y2
		strokeDashArray: bind { if (premise.exception) [6.0, 6.0] else [1.0] }
		stroke: bind selectionColor
		strokeWidth: bind selectedEdgeWidth;
    }

    // The negation bar should be a line but the line won't rotate for some reason.
    var negation: Polygon = Polygon {
		points: bind [
			x2 - (negWidth / 2), y2 + yHeadShift,
			x2 + (negWidth / 2), y2 + yHeadShift,
			x2 + (negWidth / 2), y2 + yHeadShift + negStrokeWidth,
			x2 - (negWidth / 2), y2 + yHeadShift + negStrokeWidth]
		transforms: bind getHeadRotation(x1, x2, y1, y2)
		stroke: bind stroke
		strokeWidth: bind negStrokeWidth
    }

    // exceptionhead currently not in content because of dashed lines
    var exceptionHead: Circle = Circle {
		centerX: bind x2
		centerY: bind y2
		radius: bind radius
		fill: Color.WHITE
		stroke: Color.BLACK
		visible: bind premise.exception
    }

	override function toSVG(d: XWDocument) {
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
					name: "line"
					document: d
					attributes: [
						XWAttribute {
							name: "x1"
							value: "{x1}"
						},
						XWAttribute {
							name: "y1"
							value: "{y1}"
						},
						XWAttribute {
							name: "x2"
							value: "{x2}"
						},
						XWAttribute {
							name: "y2"
							value: "{y2}"
						},
						XWAttribute {
							name: "stroke-width"
							value: "{strokeWidth}"
						},
						XWAttribute {
							name: "stroke"
							value: "{toSVGColorCode(stroke)}"
						},
						if (premise.exception) XWAttribute {
								name: "stroke-dasharray"
								value: "6.0, 6.0"
						} else null
					]
				},
				if (premise.negative) XWElement {
					name: "polygon"
					document: d
					attributes: [
						XWAttribute {
							name: "fill"
							value: "{toSVGColorCode(stroke)}"
						},
						XWAttribute {
							name: "points"
							value: "{x2 - (negWidth / 2)} {y2 + yHeadShift} {x2 + (negWidth / 2)} {y2 + yHeadShift} {x2 + (negWidth / 2)} {y2 + yHeadShift + negStrokeWidth} {x2 - (negWidth / 2)} {y2 + yHeadShift + negStrokeWidth}"
						},
						XWAttribute {
							name: "stroke-width"
							value: "{negStrokeWidth}"
						},
						XWAttribute {
							name: "stroke"
							value: "{toSVGColorCode(stroke)}"
						},
						XWAttribute {
							name: "transform"
							value: "rotate({getHeadRotationAngle(x1, x2, y1, y2)} {x2} {y2})"
						},
					]
				} else null
			]
		}
	}

    override function create():Node {
		Group {
			content: bind [
				edgeLine,
				{if (selected) selection else null},
				selectPoly,
				{if (premise.negative) negation else null},
			] // content
		} // Group
    } // composeNode
}

