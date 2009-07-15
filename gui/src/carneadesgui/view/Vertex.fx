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

// general imports
import java.lang.Math;

// scenegraph imports
import javafx.scene.*;
import javafx.scene.text.*;
import javafx.scene.paint.*;
import javafx.scene.shape.Rectangle;

// control imports
import carneadesgui.control.CarneadesControl;

// constants import
import carneadesgui.GC.*;

/**
 * The base class for tree vertices.
 */
public class Vertex extends GraphElement {

	/**
	 * The application's control object.
	 */
	public var control: CarneadesControl;

	/**
	 * The title of the vertex, if displayed.
	 */
	public var caption = "";

	public var text: Text = Text {
					content: bind caption
					textAlignment: TextAlignment.CENTER
					textOrigin: TextOrigin.BASELINE
					x: bind x
					y: bind y
				} // Text

	/**
	 * The parent vertex. null if the vertex is the root. It is called "parentVertex" since "parent" is taken by JavaFX internally.
	 */
	public var parentVertex: Vertex = null;

	/**
	 * X axis ccordinate shift relative to the parent vertex.
	 */
	public var xShift: Number = 0; // display coordinates relative to the parent node

	/**
	 * Y axis coordinate shift relative to the parent vertex.
	 */
	public var yShift: Number = 0;

	/**
	 * Absolute X coordinate. Bound and thus read-only.
	 */
	public var x: Number = 0;

	/**
	 * Absolute Y coordinate. Bound and thus read-only.
	 */
	public var y: Number = 0;

	/**
	 * The default width of the vertex. If enforced statically the moment the scaleWithText var is false.
	 */
	public var defaultWidth: Number = vertexDefaultWidth;

	/**
	 * Determines whether the vertex scales with the text.
	 */
	public var scaleWithText: Boolean = scaleVerticesWithText;

	/**
	 * The width of the vertex. Should not be set statically (use defaultWidth instead), but rather is set by a function that chooses between defaultWidth or a wider size depending on the text contained in the vertex.
	 */
	public var width: Number = { if (scaleWithText) Math.max(50, this.text.boundsInLocal.width + 10) else defaultWidth };

	/**
	 * The height of the vertex.
	 */
	public var height: Number = vertexDefaultHeight;

	/**
	 * The child vertices sequence.
	 */
	public var children: Vertex[];

	/**
	 * The depth level of the vertex, where the root is level 0.
	 */
	public var level: Number = 0; // The depth level of the vertex, where the root vertex has level 0

	// attributes for graph drawing - Don't set manually! The layout computes them.
	public var priority: Number = 0;
	public var xSubTreeSize: Number;
	public var ySubTreeSize: Number;
	public var leftSTOutline: Number;
	public var rightSTOutline: Number;
	public var bottomBrink: Number = 0;

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
		p("{caption} x/y: {x} / {y} P: {parentVertex.caption}: {level} #C: {sizeof children}");
		p("Width: {width} Height: {height} Index: {index}" );
		p("xSubtreeSize: {xSubTreeSize} ySubTreeSize: {ySubTreeSize}");
	}
}

