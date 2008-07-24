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


package Carneades.Graph.Elements;

import javafx.scene.geometry.*;
import javafx.scene.paint.*;
import javafx.scene.transform.*;
import javafx.scene.effect.DropShadow;
import javafx.scene.text.*;
import javafx.scene.*;
import javafx.input.*;

import java.lang.System;
import java.lang.Math;

// import the necessary parts of the model
import Carneades.Argument.Argument;
import Carneades.Argument.Argument.*;

// Other View Imports
import Carneades.Graph.*;

// Abstract Controller Import
import Carneades.Control.GraphControl;

public abstract class ArgumentElement extends Vertex {
	attribute fill: Color = Color.WHITE;
	override attribute text = Text {
					content: bind caption
					verticalAlignment: VerticalAlignment.TOP
					horizontalAlignment: HorizontalAlignment.CENTER
					x: bind x
					y: bind y
				} // Text

	attribute selection: Node = Rectangle {
					x: bind x - (width / 2) - 5
					y: bind y - (height / 2) - 5
					width: bind width + 10
					height: bind height + 10
					stroke: bind {if (control.dragging) GC.dragColor else GC.selectionColor};
					strokeWidth: 2
					// todo: the line below causes the Pierson v Post example to crash performance.
					// This must be a runtime issue.
					visible: bind selected
				} // selection rect
	
	attribute middlePoint: Circle = Circle {
		centerX: bind x
		centerY: bind y
		radius: 3
		fill: Color.RED
		visible: bind GC.drawDebug
	}

}

public class ArgumentBox extends ArgumentElement {
	public attribute argument: Argument;
	override attribute height = GC.argumentCircleDefaultRadius * 2;
	override attribute defaultWidth = GC.argumentCircleDefaultRadius * 2;
	override attribute scaleWithText = false;
	override attribute caption = bind argument.id;
	override attribute fill = bind {if (argument.ok) Color.LIGHTGREY else Color.WHITE};
	override attribute bottomBrink = GC.argumentBoxBottomBrink;

	override attribute text = Text {
					content: bind { 
						if ((argument.conclusion.standard) instanceof BestArgument)
							"{if (argument.pro) '+' else '-'}.{(argument.weight * 100) as Integer}" 
							else "{if (argument.pro) '+' else '-'}"}
					verticalAlignment: VerticalAlignment.TOP
					horizontalAlignment: HorizontalAlignment.CENTER
					x: bind x
					// The text Y coordinate positioning is dirty as the text currently lacks a currentheight attribute
					y: bind y + (text.font.size / 3)
					font: Font {
			/*size: { if ((argument.conclusion.standard) instanceof BestArgument) GC.canvasFontSize 
			  else (GC.canvasFontSize + 10) }*/
						style: FontStyle.BOLD
					}
				} // Text

	private attribute mainCircle: Circle = Circle {
		centerX: bind x
		centerY: bind y
		radius: bind GC.argumentCircleDefaultRadius

		fill: bind {
			if (not argument.ok) Color.WHITE
			else {
				if (not argument.pro) GC.argumentConColor else GC.argumentProColor
			}
		}; 
		
		stroke: Color.BLACK
		blocksMouse: true

		effect: { 
			if (GC.drawShadows) {
				DropShadow {
					color: bind GC.shadowColor
					offsetX: bind GC.xShadowShift
					offsetY: bind GC.yShadowShift
					radius: bind GC.shadowBlurRadius
				}
			} else null
		}

		onMouseClicked: function(e: MouseEvent) {
			control.unSelectAll();
			selected = true;
			control.processSelection();

			if (GC.debug) {
				print();
			}
		}

		onMouseDragged: function(e: MouseEvent) {
			if (this.selected) { control.startDrag(); }
		}	

		onMouseReleased: function(e: MouseEvent) {
			if (control.dragging) {	control.endDrag(); }
		}

		onMouseEntered: function(e: MouseEvent) {
			if (control.dragging) { control.setDraggingOver(this); }
		}

		onMouseExited: function(e: MouseEvent) {
			if (control.dragging) { control.setDraggingOver(null); }
		}
	}

	override attribute selection = Circle {
		centerX: bind x 
		centerY: bind y
		radius: GC.argumentCircleDefaultRadius + 5
		stroke: bind {if (control.dragging) GC.dragColor else GC.selectionColor};
		strokeWidth: 2
		visible: bind selected
	} // selection circle


	public function create():Node {
		Group {
			content: [
				mainCircle, 
				selection,
				text,
				middlePoint
			] // content
		} // Group
	} // composeNode
}

public class StatementBox extends ArgumentElement {
	public attribute statement: Statement;
	override attribute scaleWithText = false;
	override attribute defaultWidth = GC.statementBoxDefaultWidth;
	override attribute bottomBrink = GC.statementBoxBottomBrink;
	override attribute caption = bind { if (statement.wff.length() < GC.numDisplayedChars) statement.wff
										else "{statement.wff.substring(0, GC.numDisplayedChars-1)}..."};
	private attribute status: String = bind statement.getBoundStatus();
	private attribute statusColor = bind { 
		if (status == "stated") GC.statusStatedColor
		else if (status == "assumed true") GC.statusAssumedTrueColor
		else if (status == "assumed false") GC.statusAssumedFalseColor
		else if (status == "rejected") GC.statusRejectedColor
		else if (status == "accepted") GC.statusAcceptedColor
		else /*if (status == "questioned")*/ GC.statusQuestionedColor
	}

	attribute mainRect: Rectangle = Rectangle {
					x: bind x - (width / 2)
					y: bind y - (height / 2)
					width: bind width 
					height: bind height
					fill: bind { if (GC.fillStatements) statusColor else GC.defaultBoxFill }
					stroke: bind { if (GC.fillStatements) Color.BLACK else statusColor }
					strokeWidth: 1

					effect: { 
						if (GC.drawShadows) {
							DropShadow {
								color: bind GC.shadowColor
								offsetX: bind GC.xShadowShift
								offsetY: bind GC.yShadowShift
								radius: bind GC.shadowBlurRadius
							}
						} else null
					}
				
					onMouseClicked: function(e: MouseEvent) {
						control.unSelectAll();
						selected = true;
						control.processSelection();
						
						if (GC.debug) {
							print();
						}
					}

					onMouseDragged: function(e: MouseEvent) {
						if (this.selected) { control.startDrag(); }
					}	

					onMouseReleased: function(e: MouseEvent) {
						if (control.dragging) {	control.endDrag(); }
					}

					onMouseEntered: function(e: MouseEvent) {
						if (control.dragging) { control.setDraggingOver(this); }
					}

					onMouseExited: function(e: MouseEvent) {
						if (control.dragging) { control.setDraggingOver(null); }
					}	
				} // main rect

	private attribute acceptableCircle: Circle = Circle {
		centerX: bind x + (this.width / 2) + GC.acceptableCirclePadding + (GC.acceptableCircleWidth / 2)
		centerY: bind y - (GC.acceptableCirclePadding / 2) - (GC.acceptableCircleWidth / 2)
		radius: bind (GC.acceptableCircleWidth / 2)
		fill: bind { if (statement.ok) GC.statusAcceptedColor else null }
		strokeWidth: 1
		stroke: Color.BLACK

		effect: { 
			if (GC.drawShadows) {
				DropShadow {
					color: bind GC.shadowColor
					offsetX: bind GC.xShadowShift
					offsetY: bind GC.yShadowShift
					radius: bind GC.shadowBlurRadius
				}
			} else null
		}
	}

	private attribute acceptableCompCircle: Circle = Circle {
		centerX: bind x + (this.width / 2) + GC.acceptableCirclePadding + (GC.acceptableCircleWidth / 2)
		centerY: bind y + (GC.acceptableCirclePadding / 2) + (GC.acceptableCircleWidth / 2)
		radius: bind (GC.acceptableCircleWidth / 2)
		fill: bind { if (statement.complementOk) GC.statusRejectedColor else null }
		strokeWidth: 1
		stroke: Color.BLACK
		effect: { 
				if (GC.drawShadows) {
					DropShadow {
					color: bind GC.shadowColor
					offsetX: bind GC.xShadowShift
					offsetY: bind GC.yShadowShift
					radius: bind GC.shadowBlurRadius
				}
			} else null
		}
	}

	public function create():Node {
		Group {
			content: [
				mainRect,
			    selection,
				text,
				acceptableCircle,
				acceptableCompCircle,
				middlePoint
			] // content
		} // Group
	} // composeNode
}

public class Arrow extends Edge {
	attribute headSize: Number = 10;
	attribute heading: Number = 0; // 0 means it points right, 1 it points left, straight up or down does not matter
	attribute fill: Color = Color.BLACK;
	override attribute stroke = Color.BLACK;
	override attribute turnHead = true;

	postinit {
		setAngle();
	}

	public function create():Node {
		Group {
			content: [
				line,
				Polygon {
					transform: bind [Transform.rotate(angle, x2, y2)]

					points: bind [ x2, y2, 
								x2 - headSize, y2+(headSize / 2),
								x2 - headSize, y2-(headSize / 2)]
					stroke: bind stroke
					strokeWidth: bind strokeWidth
					fill: bind fill
				}
			] // content
		} // group
	}
}

public class ArgumentLink extends Arrow {
	override attribute headSize = 10;
}

public class ConArgumentLink extends ArgumentLink {
	override attribute yHeadShift = bind headSize / 3;
	override attribute fill = Color.WHITE
}

public class ProArgumentLink extends ArgumentLink {
	override attribute yHeadShift = bind headSize / 3;
	override attribute fill = Color.BLACK
}

public class PremiseLink extends Edge {
	public attribute premise: Premise;
	attribute radius: Number = 5;
	attribute negWidth: Number = 10;
	attribute negStrokeWidth: Number = 2;
	override attribute dashed = bind premise.exception;

	public attribute negated: Boolean = false;
	override attribute turnHead = bind negated;
	
	attribute negation: Line = Line {
					startX: bind x2 - Math.max(yHeadShift * 2, negWidth)
					startY: bind y2 - (negWidth / 2) 
					endX: bind x2 - Math.max(yHeadShift * 2, negWidth)
					endY: bind y2 + (negWidth / 2)
					transform: bind [Transform.rotate(angle, x2, y2)]
					stroke: bind stroke
					strokeWidth: bind negStrokeWidth
					visible : bind negated
				}

	// exceptionhead currently not in content because of dashed lines
	attribute exceptionHead: Circle = Circle {
   				     centerX: bind x2
   				     centerY: bind y2
   	    			 radius: bind radius
   				     fill: Color.WHITE
   				     stroke: Color.BLACK
					 visible: bind premise.exception
    			}

	public function negate() {
		if (negated) {
			negated = false;
		} else {
			negated = true;
		}
	}

	public function create():Node {
		Group {
			content: [
				line, negation
			] // content
		} // Group
	} // composeNode
}

