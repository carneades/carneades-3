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

package carneadesgui;

import javafx.scene.paint.*;
import java.lang.System;
import java.lang.Object;

import javafx.scene.shape.Circle;

import javafx.scene.input.MouseEvent;

import javafx.scene.paint.Color;
import javafx.scene.shape.Rectangle;
import javafx.scene.text.Font;
import javafx.scene.text.Text;
import javafx.scene.text.TextAlignment;
import javafx.scene.text.TextOrigin;

// Model Constants
public var proofStandardSE: String = "scintilla of evidence";
public var proofStandardDV: String = "dialectical validity";
public var proofStandardBA: String = "best argument";
public var proofStandardPE: String = "preponderance";
public var proofStandardCCE: String = "clear and convincing";
public var proofStandardBRD: String = "beyond reasonable doubt";

// General Application Constants
public var appWidth: Number = 800;
public var appHeight: Number = 600;
public var verticalWindowMismatch: Number = 23;
public var horizontalWindowMismatch: Number = 1;

// StandardView Constants
public var mainPanelSpacing: Integer = 1;	// spacing between the two/three main panels

// Graph Display Constants
//
// Alignment constants for edge ending offsets
// The variable gives information about from which direction the link hits the node
public var TOP: Number = 1;
public var BOTTOM: Number = 2;
public var RIGHT: Number = 3;
public var LEFT: Number = 4;

// Drawing constants
public var drawAllStatements: Boolean = true;
public var canvasFontSize: Integer = 8;
public var textFieldHeight: Integer = 25;
public var defaultBoxFill = Color.WHITE;
public var drawDebug = bind debug;
public var viewBackground: Color = Color.rgb(178, 195, 218);
public var toolPanelBackground: Color = Color.rgb(124, 141, 172);
public var panelBackground: Color = Color.rgb(223, 226, 229);

public var edgeStrokeWidth: Number = 1;
public var selectionColor: Color = Color.RED;
public var hoverColor: Color = Color.ORANGE;
public var dragColor: Color = Color {
												red: 0.5
												green: 0
												blue: 0
												opacity: 0.5
												};
public var possibleColor: Color = Color.GREEN;
public var impossibleColor: Color = Color.RED;

public var edgeSelectionWidth: Integer = 5;
public var selectedEdgeWidth: Integer = 2;

// shadows
public var drawShadows: Boolean = true;
public var shadowColor: Color = Color.rgb(0, 0, 0, 0.7 );
public var shadowBlurRadius: Integer = 4;
public var xShadowShift: Integer = 4;
public var yShadowShift: Integer = 6;

// animations
public var zoomTime: Number = 4.0; // in seconds
public var zoomLimits: Number[] = [0.2, 3.0];

// General Vertex Constants
public var scaleVerticesWithText = false;
public var vertexDefaultWidth: Integer = 50;
public var vertexDefaultHeight: Integer = 50;

// Statement Boxes
public var numDisplayedChars = 15;
public var statementBoxDefaultWidth: Integer = 150;
public var statementBoxBottomBrink: Integer = 50;
public var boxTextPadding: Integer = 5;

public var fillStatements = true;
public var statusAcceptedColor: Color = Color.rgb(45, 193, 56);
public var statusRejectedColor: Color = Color.rgb(255, 82, 85);
public var statusAssumedTrueColor: Color = Color.rgb(106, 255, 121);
public var statusAssumedFalseColor: Color = Color.rgb(193, 160, 164);
public var statusStatedColor: Color = Color.WHITE;
public var statusQuestionedColor: Color = Color.rgb(255, 251, 144);

public var displayAcceptableCircles: Boolean = true;
public var acceptableCircleWidth: Integer = 15;
public var acceptableCirclePadding: Integer = 5;

// Argument Boxes. They were originally boxes, hence the name.
public var argumentBoxDefaultWidth: Integer = 60;
public var argumentBoxBottomBrink: Integer = 3;
public var argumentCircleDefaultRadius: Integer = 20;
public var argumentConColor = statusRejectedColor;
public var argumentProColor = statusAcceptedColor;

// toolbar constants
public var toolBarHeight: Integer = 60;
public var toolBarSpacing: Integer = 10;

// inspector panel constants
public var inspectorPanelWidth: Integer = 220;
public var inspectorLabelWidth: Integer = 80;
public var inspectorDefaultMode: Integer = 0;
public var inspectorStatementMode: Integer = 1;
public var inspectorArgumentMode: Integer = 2;
public var inspectorPremiseMode: Integer = 3;
public var inspectorGraphMode: Integer = 4;

// Graph List mode constants
public var listGraphMode: Integer = 1;
public var listStatementMode: Integer = 2;
public var listArgumentMode: Integer = 3;
public var listViewHeight: Integer = 160;
public var listEntryFieldHeight: Integer = 20;
public var tabButtonHeight: Integer = 40;

// Edit button panel constants
public var editButtonPanelHeight: Integer = 50;

// User interaction constants
public var controlsLocked: Boolean = false;
public var idsEditable: Boolean = false;

// Version administration constants
public var debug: Boolean = false;

// Graph Layout constants

/**
 * The horizontal distance between neighboring tree nodes.
 */
public var xDistance: Integer = bind acceptableCircleWidth + (2 * acceptableCirclePadding) + 10;

/**
 * The vertical distance between the layers of the tree.
 */
public var yDistance: Integer = 50;

// Graph validation constants

/**
 * Constant for functions to return if their operation performed valid.
 */
public var AG_OK: Number = 1;

/**
 * Constant for validation functions to return if graph contains a cycle.
 */
public var AG_CYCLE: Number = 2;

/**
 * Constants for validation functions to return if graph contains a double id.
 */
public var AG_DOUBLE_ID: Number = 3;

// Command execution constants

/**
 * Constant for commands to return if their execution was successful.
 */
public var C_OK: Number = 0;

/**
 * Constant for commands to return if their execution was not successful.
 */
public var C_ERROR: Number = 1;

/**
 * Constant for commands to return if no undo is possible.
 */
public var C_NO_UNDO: Number = 2;

/**
 * Constants for commands if no further redo is possible.
 */
public var C_LATEST_COMMAND: Number = 3;

// helper functions

/**
 * Shortcut print function for debug purposes.
 */
public var p = function(s: String) { System.out.println(s)}

/**
 * Function returning true if a certain element is contained in the list or not. This function checks for equality by comparing the pointers to the objects, not the object content itself.
 */
public var contains = function(list: Object[], element: Object): Boolean {
	for (i in list) {
		if (i == element) { return true; }
	}
	return false;
}

public var flag = Circle {
	centerX: 0
	centerY: 0
	radius: 5
	fill: Color.PINK
	onMouseClicked: function(e:MouseEvent): Void {
		p("c");
	}
}

public class Filler extends Text {
	public var text = "This \nis a \ntotally cool \nfiller text...";

	override var textAlignment = TextAlignment.LEFT;
	override var textOrigin = TextOrigin.TOP;
	override var font = Font { size: 12 };
	override var content = bind text;
}

public class LayoutRect extends Rectangle {
	override var x = 0;
	override var y = 0;
	override var width = bind parent.layoutBounds.width;
	override var height = bind parent.layoutBounds.height;
	override var fill = bind Color.RED;
	//override var stroke = Color.YELLOW;
}

