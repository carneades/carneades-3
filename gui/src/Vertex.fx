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

// general imports
import java.lang.Math;
import java.lang.System;

// scenegraph imports
import javafx.scene.*;
import javafx.scene.geometry.*;
import javafx.scene.text.*;
import javafx.scene.paint.*;

// control imports
import Carneades.Control.GraphControl;

// view imports
import Carneades.Graph.*;
import Carneades.Graph.GC.*;

/**
 * The base class for tree vertices.
 */
public class Vertex extends GraphElement {

	/**
	 * The title of the vertex, if displayed.
	 */
	attribute caption = "";

	/**
	 * The parent vertex. null if the vertex is the root. It is called "parentVertex" since "parent" is taken by JavaFX internally.
	 */
	public attribute parentVertex: Vertex = null;

	/**
	 * X axis ccordinate shift relative to the parent vertex.
	 */
	public attribute xShift: Number = 0; // display coordinates relative to the parent node

	/**
	 * Y axis coordinate shift relative to the parent vertex.
	 */
	public attribute yShift: Number = 0; 

	/**
	 * Absolute X coordinate. Bound and thus read-only.
	 */
	public attribute x: Number = bind parentVertex.x + xShift; 

	/**
	 * Absolute Y coordinate. Bound and thus read-only.
	 */
	public attribute y: Number = bind parentVertex.y + yShift;

	/**
	 * The default width of the vertex. If enforced statically the moment the scaleWithText attribute is false.
	 */
	public attribute defaultWidth: Number = vertexDefaultWidth;

	/**
	 * Determines whether the vertex scales with the text.
	 */
	public attribute scaleWithText: Boolean = scaleVerticesWithText;
	
	/**
	 * The width of the vertex. Should not be set statically (use defaultWidth instead), but rather is set by a function that chooses between defaultWidth or a wider size depending on the text contained in the vertex.
	 */
	attribute width: Number = { if (scaleWithText) Math.max(50, text.getWidth() + 10) else defaultWidth };

	/**
	 * The height of the vertex.
	 */
	public attribute height: Number = vertexDefaultHeight;

	/**
	 * The child vertices sequence.
	 */
	public attribute children: Vertex[];

	/**
	 * The depth level of the vertex, where the root is level 0.
	 */
	public attribute level: Number = 0; // The depth level of the vertex, where the root vertex has level 0

	// attributes for graph drawing - Don't set manually! The layout computes them.
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
	
	override function create():Node {
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
	
	/**
	 * Print information about the vertex for debugging purposes.
	 */
	public function print(): Void {
		p("Clicked: {caption} x/y: {x} / {y} P: {parentVertex.caption}: {level} #C: {sizeof children}");
		p("Width: {width} Height: {height} Index: {index}" );
		p("xSubtreeSize: {xSubTreeSize} ySubTreeSize: {ySubTreeSize}");
	}
}

