/*
 * StatementBox.fx
 *
 * Created on 27.10.2009, 19:59:46
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
import carneadesgui.view.Elements.*;
import carneadesgui.GC.*;

// control import
import javafx.scene.Node;
import javafx.scene.input.MouseEvent;
import javafx.scene.paint.Color;
import javafx.scene.shape.Circle;
import javafx.scene.shape.Rectangle;
import javafx.scene.shape.Polygon;
import javafx.scene.transform.Translate;
import javafx.scene.text.TextAlignment;
import javafx.scene.text.TextOrigin;
import javafx.scene.input.MouseButton;
import javafx.scene.image.Image;
import javafx.scene.image.ImageView;

import carneadesgui.control.XWDocumentBuilder.XWText;
import carneadesgui.control.XWDocumentBuilder.XWElement;
import carneadesgui.control.XWDocumentBuilder.XWDocument;
import carneadesgui.control.XWDocumentBuilder.XWAttribute;

/**
 * The statement view object.
 */
public class StatementBox extends ArgumentElement {

    /**
     * The represented model statement.
     */
    public var statement: Statement;

	// Is this the primary box of the statement
	public var duplicate: Boolean = false;

    // bind the model element to the statement element
    override def model = bind statement;

	override def level = bind { parentVertex.level + 1}

	override def cache = true;
	override var width = statementBoxDefaultWidth;
	override var height = statementBoxDefaultHeight;
	override def bottomBrink = statementBoxBottomBrink;

    override def caption = bind statement.wff on replace { (text as CenteredStatementText).changeText(statement.wff) }

    def statusColor = bind {
		if (statement.status == "stated") statusStatedColor
		else if (statement.status == "assumed true") statusAssumedTrueColor
		else if (statement.status == "assumed false") statusAssumedFalseColor
		else if (statement.status == "rejected") statusRejectedColor
		else if (statement.status == "accepted") statusAcceptedColor
		else /*if (status == "questioned")*/ statusQuestionedColor
    };

	// main statement box
	def mainRectWidth = width - acceptableCircleWidth;
	def mainRectHeight = height;
    def mainRect: Polygon = Polygon {
		points: bind [
			x - (width / 2), y - (height / 2),
			x - (width / 2) + mainRectWidth, y - (height / 2),
			x - (width / 2) + mainRectWidth, y - (height / 2) + mainRectHeight,
			x - (width / 2), y - (height / 2) + mainRectHeight ]
		blocksMouse: true
		fill: bind { if (fillStatements) statusColor else DEFAULT_BOX_FILL }
		stroke: bind { if (mainRect.hover) Color.GREY else Color.BLACK }
		strokeDashArray: bind {if (duplicate) [6.0, 6.0] else [1.0]}
		strokeWidth: 1

		onMouseClicked: function(e: MouseEvent): Void {
			control.processGraphSelection(this);
		}

		onMouseDragged: function(e: MouseEvent) {
			if (this.selected and (e.button == MouseButton.PRIMARY)) { control.startDrag(this); }
		}

		onMouseReleased: function(e: MouseEvent) {
			if (control.dragging) { control.endDrag(e.shiftDown); }
		}

		onMouseEntered: function(e: MouseEvent) {
			if (control.dragging) { control.setDraggingOver(this); }
		}

		onMouseExited: function(e: MouseEvent) {
			if (control.dragging) { control.setDraggingOver(null); }
		}
    } // main rect

	def text: CenteredStatementText = CenteredStatementText {
		blocksMouse: false
		x: bind x - (acceptableCircleWidth/ 2)
		y: bind y
		content: toEscapeLines(caption)
    } // Text

    def acceptableCircle: Circle = Circle {
		centerX: bind x + (this.width / 2) - (acceptableCircleWidth / 2) + 3
		centerY: bind y - (acceptableCirclePadding / 2) - (acceptableCircleWidth / 2)
		radius: bind (acceptableCircleWidth / 2)
		fill: bind { if (statement.ok) statusAcceptedColor else null }
		strokeWidth: 1
		stroke: Color.BLACK
    }

    def acceptableCompCircle: Circle = Circle {
		centerX: bind x + (this.width / 2) - (acceptableCircleWidth / 2) + 3
		centerY: bind y + (acceptableCirclePadding / 2) + (acceptableCircleWidth / 2)
		radius: bind (acceptableCircleWidth / 2)
		fill: bind { if (statement.complementOk) statusRejectedColor else null }
		strokeWidth: 1
		stroke: Color.BLACK
	}

	def duplicateLink: Group = Group {
		def dimension: Integer = 20
		translateX: bind this.x - this.width / 2
		translateY: bind this.y + this.height / 2 - dimension
		content: [
			Rectangle {
				def stroke: Number = 1
				width: dimension
				height: dimension
				fill: null
				stroke: Color.BLUE
				strokeWidth: 1
			},
			ImageView {
				fitWidth: dimension
				fitHeight: dimension
				image: Image {
					url: "{__DIR__}images/icon-link.png"
				}
			}
		]
	}

    override function create():Node {
		Group {
			content: bind [
				mainRect,
				selection,
				acceptableCircle,
				acceptableCompCircle,
				{ if (duplicate) duplicateLink else null},
				text
				// middlePoint,
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
					name: "rect"
					document: d
					attributes: [
						XWAttribute {
							name: "x"
							value: "{x}"
						},
						XWAttribute {
							name: "y"
							value: "{y}"
						},
						XWAttribute {
							name: "transform"
							value: "translate(-{width/2}, -{height/2})"
						},
						XWAttribute {
							name: "width"
							value: "{mainRectWidth}"
						},
						XWAttribute {
							name: "height"
							value: "{mainRectHeight}"
						},
						XWAttribute {
							name: "fill"
							value: "{toSVGColorCode(mainRect.fill)}"
						},
						XWAttribute {
							name: "opacity"
							value: "{mainRect.opacity}"
						},
						XWAttribute {
							name: "stroke"
							value: "{toSVGColorCode(mainRect.stroke)}"
						},
						XWAttribute {
							name: "stroke-width"
							value: "{mainRect.strokeWidth}"
						},
					]
				},
				XWElement {
					name: "circle"
					document: d
					attributes: [
						XWAttribute {
							name: "cx"
							value: "{x + (this.width / 2) - (acceptableCircleWidth / 2) + 3}"
						},
						XWAttribute {
							name: "cy"
							value: "{y - (acceptableCirclePadding / 2) - (acceptableCircleWidth / 2)}"
						},
						XWAttribute {
							name: "r"
							value: "{acceptableCircleWidth / 2}"
						},
						XWAttribute {
							name: "fill"
							value: "{if (statement.ok) toSVGColorCode(statusAcceptedColor) else "none"}"
						},
						XWAttribute {
							name: "opacity"
							value: "{acceptableCircle.opacity}"
						},
						XWAttribute {
							name: "stroke"
							value: "{toSVGColorCode(acceptableCircle.stroke)}"
						},
						XWAttribute {
							name: "stroke-width"
							value: "{acceptableCircle.strokeWidth}"
						},
					]
				},
				XWElement {
					name: "circle"
					document: d
					attributes: [
						XWAttribute {
							name: "cx"
							value: "{x + (this.width / 2) - (acceptableCircleWidth / 2) + 3}"
						},
						XWAttribute {
							name: "cy"
							value: "{y + (acceptableCirclePadding / 2) + (acceptableCircleWidth / 2)}"
						},
						XWAttribute {
							name: "r"
							value: "{acceptableCircleWidth / 2}"
						},
						XWAttribute {
							name: "fill"
							value: "{if (statement.complementOk) toSVGColorCode(statusRejectedColor) else "none"}"
						},
						XWAttribute {
							name: "opacity"
							value: "{acceptableCompCircle.opacity}"
						},
						XWAttribute {
							name: "stroke"
							value: "{toSVGColorCode(acceptableCompCircle.stroke)}"
						},
						XWAttribute {
							name: "stroke-width"
							value: "{acceptableCompCircle.strokeWidth}"
						},
					]
				},
				XWElement {
					document: d
					name: "text"
					attributes: [
						XWAttribute {
							name: "x"
							value: "{text.x
										+ (text.transforms[0] as Translate).x
										- acceptableCircleWidth / 2
										+ SVG_STATEMENTBOX_TEXT_HOR_HANDCOR}"
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
						for (l in toLines(statement.wff))
							XWElement {
								document: d
								name: "tspan"
								attributes: [
									XWAttribute {
										name: "x"
										value: "0"
									},
									XWAttribute {
										name: "dy"
										value: "1em"
									},
								]
								children: XWText {
									value: l
								}
							}
					]
				}
			]
		}
	}
}
