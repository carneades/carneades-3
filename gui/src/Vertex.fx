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


package GraphSketch1.Graph;

import java.lang.Math;
import javafx.gui.*;
import GraphSketch1.Graph.*;
import java.lang.System;
import GraphSketch1.Control.AbstractGraphControl;

public class Vertex extends GraphElement {
	// The content object that is represented by the vertex
	// unclear: Does the plain java object type work here? Test to come ...
	// Right now String for debugging purposes
	attribute caption = "";
	// It is called "parentVertex" since "parent" is taken by JavaFX internally.
	attribute parentVertex: Vertex = null;
	attribute xShift: Number = 0; // display coordinates relative to the parent node
	attribute yShift: Number = 0; 
	attribute x: Number = bind parentVertex.x + xShift; // the absolute coordinates
	attribute y: Number = bind parentVertex.y + yShift; // in case of the root vertex, it gets assigned 0 by default
	attribute width: Number = Math.max(50, text.getWidth() + 10); // default display dimensions
	attribute height: Number = 50;
	attribute children: Vertex[];
	attribute level: Number = 0; // The depth level of the vertex, where the root vertex has level 0

	// attributes for graph drawing - Don't set manually!
	attribute priority: Number = 0;
	attribute xSubTreeSize: Number;
	attribute ySubTreeSize: Number;
	attribute leftSTOutline: Number;
	attribute rightSTOutline: Number;

	attribute control: AbstractGraphControl;
	
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
					x: bind x - (width / 2).intValue()
					y: bind y
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

	public function print():Void {
		System.out.println(content + ":");
		System.out.println(content + " level: " + level);
		System.out.println(content + " x/y shift: " + xShift +" / " + yShift);
		//System.out.println(content + " children: " + children);
		System.out.println(content + " priority: " + priority);
		System.out.println(content + " X Sub Tree Size: " + xSubTreeSize);
	}
}

