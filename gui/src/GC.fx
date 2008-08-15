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

import javafx.scene.paint.*;
import java.lang.System;
import java.lang.Object;

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
public var transparent: Color = Color {
					red: 0
					green: 0
					blue: 0
					opacity: 0
				}

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

// General Vertex Constants
public var scaleVerticesWithText = false;
public var vertexDefaultWidth: Integer = 50;
public var vertexDefaultHeight: Integer = 50;

// Statement Boxes
public var numDisplayedChars = 15;
public var statementBoxDefaultWidth: Integer = 150;
public var statementBoxBottomBrink: Integer = 50;

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

// Argument Boxes
public var argumentBoxDefaultWidth: Integer = 60;
public var argumentBoxBottomBrink: Integer = 3;
public var argumentCircleDefaultRadius: Integer = 20;
public var argumentConColor = statusRejectedColor;
public var argumentProColor = statusAcceptedColor;

// Application constants
public var appWidth: Integer = 1000;
public var appHeight: Integer = 720;

// Layout constants
public var xOffset: Integer = appWidth / 2;
public var yOffset: Integer = 0;
public var xDistance: Integer = bind acceptableCircleWidth + (2 * acceptableCirclePadding) + 10;
public var yDistance: Integer = 50;

var drawNonArguedStatements = false;

// Graph validation constants
public var AG_OK: Number = 1;
public var AG_CYCLE: Number = 2;
public var AG_DOUBLE_ID: Number = 3;

// Command execution constants
public var C_OK: Number = 0;
public var C_ERROR: Number = 1;
public var C_NO_UNDO: Number = 2;
public var C_LATEST_COMMAND: Number = 3;

// window layout onstants
public var graphListWidth: Integer = 100;
public var editWidth: Integer = 330;
public var editHeight: Integer = 410;
public var listHeight: Integer = 300;
public var editLabelWidth: Integer = 100;
public var toolBarHeight: Integer = 60;

public var toolBarButtonHeight: Integer = 60;
public var toolBarButtonWidth: Integer = 60;

// User interaction constants
public var idsEditable: Boolean = false;

// Version administration constants
public var debug: Boolean = false;

// helper functions
public var p = function(s: String) { System.out.println(s)}

// HELPER FUNCTIONS plus public variables
public var contains = function(list: Object[], element: Object): Boolean {
	for (i in list) {
		if (i == element) { return true; }
	}
	return false;
}

