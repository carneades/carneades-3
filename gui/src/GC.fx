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

// Class for Constants

public class GC {

	// Alignment constants for edge ending offsets
	// The variable gives information about from which direction the link hits the node
	public static attribute TOP: Number = 1;
	public static attribute BOTTOM: Number = 2;
	public static attribute RIGHT: Number = 3;
	public static attribute LEFT: Number = 4;

	// Drawing constants
	public static attribute drawDebug = bind debug;
	public static attribute viewBackground: Color = Color.rgb(178, 195, 218);
	public static attribute toolPanelBackground: Color = Color.rgb(124, 141, 172);
	public static attribute panelBackground: Color = Color.rgb(223, 226, 229);

	public static attribute edgeStrokeWidth: Number = 1;
	public static attribute selectionColor: Color = Color.RED;
	public static attribute hoverColor: Color = Color.ORANGE;
	public static attribute dragColor: Color = Color {
													red: 0.5
													green: 0
													blue: 0
													opacity: 0.5
													};
	public static attribute possibleColor: Color = Color.GREEN;
	public static attribute impossibleColor: Color = Color.RED;
	public static attribute transparent: Color = Color {
						red: 0
						green: 0
						blue: 0
						opacity: 0
					}
	
	public static attribute edgeSelectionWidth = 5;
	public static attribute selectedEdgeWidth = 2;

	// General Vertex Constants
	public static attribute scaleVerticesWithText = false;
	public static attribute vertexDefaultWidth = 50;

	// Statement Boxes
	public static attribute numDisplayedChars = 15;
	public static attribute statementBoxDefaultWidth = 170;

	public static attribute statusAcceptedColor: Color = Color.rgb(45, 193, 56);
	public static attribute statusRejectedColor: Color = Color.rgb(255, 82, 85);
	public static attribute statusAssumedTrueColor: Color = Color.rgb(106, 255, 121);
	public static attribute statusAssumedFalseColor: Color = Color.rgb(193, 160, 164);
	public static attribute statusStatedColor: Color = Color.WHITE;
	public static attribute statusQuestionedColor: Color = Color.rgb(255, 251, 144);
	public static attribute statusAcceptableColor: Color = Color.rgb(194, 255, 173);

	// Argument Boxes
	public static attribute argumentBoxDefaultWidth = 60;
	public static attribute argumentCircleDefaultRadius = 15;
	public static attribute defensibleStrengthColorRed: Number = 41;
	public static attribute defensibleStrengthColorGreen: Number = 206;
	public static attribute defensibleStrengthColorBlue: Number = 51;

	// Application constants
	public static attribute appWidth = 1000;
	public static attribute appHeight = 700;

	// Layout constants
	public static attribute xOffset = appWidth / 2;
	public static attribute yOffset = 0;
	public static attribute xDistance = 10;
	public static attribute yDistance = 50;

	public static attribute drawNonArguedStatements = false;

	// Graph validation constants
	public static attribute AG_OK: Number = 1;
	public static attribute AG_CYCLE: Number = 2;
	public static attribute AG_DOUBLE_ID: Number = 3;

	// Command execution constants
	public static attribute C_OK: Number = 0;
	public static attribute C_ERROR: Number = 1;
	public static attribute C_NO_UNDO: Number = 2;
	public static attribute C_LATEST_COMMAND: Number = 3;

	// Edit window constants
	public static attribute editWidth: Integer = 300;
	public static attribute editHeight: Integer = 380;
	public static attribute listHeight: Integer = 300;
	public static attribute editLabelWidth: Integer = 100;
	public static attribute toolBarHeight: Integer = 50;

	// User interaction constants
	public static attribute idsEditable: Boolean = false;

	// Version administration constants
	private static attribute version: String = "release";
	public static attribute releaseVersion: Boolean = bind version == "release";
	public static attribute debug: Boolean = bind version == "debug";

	// helper functions
	public static function p(s: String) { System.out.println(s)}
}
