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

import java.lang.Math;
import javafx.scene.*;
import javafx.scene.geometry.*;
import javafx.scene.text.*;
import javafx.scene.paint.*;
import Carneades.Graph.*;
import java.lang.System;
import Carneades.Control.GraphControl;

public class Vertex extends GraphElement {
	// The content object that is represented by the vertex
	// unclear: Does the plain java object type work here? Test to come ...
	// Right now String for debugging purposes
	attribute caption = "";
	// It is called "parentVertex" since "parent" is taken by JavaFX internally.
	attribute parentVertex: Vertex = null;
	attribute xShift: Number = 0; // display coordinates relative to the parent node
	attribute yShift: Number = 0; 
	attribute x: Number = bind parentVertex.x + xShift; 
	attribute y: Number = bind parentVertex.y + yShift;

	attribute defaultWidth: Number = GC.vertexDefaultWidth;
	attribute scaleWithText: Boolean = GC.scaleVerticesWithText;
	public attribute width: Number = { if (scaleWithText) Math.max(50, text.getWidth() + 10) else defaultWidth };

	public attribute height: Number = GC.vertexDefaultHeight;
	attribute children: Vertex[];
	attribute level: Number = 0; // The depth level of the vertex, where the root vertex has level 0

	// attributes for graph drawing - Don't set manually!
	attribute priority: Number = 0;
	attribute xSubTreeSize: Number;
	attribute ySubTreeSize: Number;
	attribute leftSTOutline: Number;
	attribute rightSTOutline: Number;
	attribute bottomBrink: Number = 0;

	attribute control: GraphControl;
	
	attribute text: Text = Text {
					content: bind caption
					verticalAlignment: VerticalAlignment.CENTER
					horizontalAlignment: HorizontalAlignment.CENTER
					x: bind x
					y: bind y
				} // Text
	
	public function create():Node {
		Group {
			content: [
				Rectangle {
					x: bind x - (width/2)
					y: bind y - (height/2)
					width: bind width
					height: bind height
					fill: Color.WHITE
					stroke: Color.BLACK
					strokeWidth: 2
					arcWidth: 5
					arcHeight: 5
					visible: bind visible
				} // Rect
				, text
			] // content
		} // Group
	} // composeNode
	
	public function print(): Void {
		GC.p("Clicked: " + caption + " x/y: " + x + " / " + y + " P: " + parentVertex.caption+ " L: " + level + " #C: " + sizeof children);
		GC.p("Width: " + width + " Height: " + height + " Index: " + index );
		GC.p("xSubtreeSize: " + xSubTreeSize + " ySubTreeSize: " + ySubTreeSize);
	}

}

