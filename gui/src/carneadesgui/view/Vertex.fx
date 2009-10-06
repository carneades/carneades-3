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

// scenegraph imports
import javafx.scene.*;
import javafx.scene.text.*;
import javafx.scene.paint.*;
import javafx.scene.shape.Rectangle;

// control imports

// constants import
import carneadesgui.GC.*;
import javafx.scene.input.MouseEvent;

import carneadesgui.control.XWDocumentBuilder.XWElement;
import carneadesgui.control.XWDocumentBuilder.XWDocument;


/**
 * The base class for tree vertices.
 */
public class Vertex extends GraphElement {

    // hide vertex by default to blend it in
    override var visible = false;

    postinit {
		for (c in children) c.parentVertex = this;
    }

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
     * Absolute X coordinate.
     */
    public var x: Number = 0;
    public var xNew: Number = 0;
    public var xOld: Number = 0;

    /**
     * Absolute Y coordinate.
     */
    public var y: Number = 0;
    public var yNew: Number = 0;
    public var yOld: Number = 0;

    /**
     * The width of the vertex. Should not be set statically (use defaultWidth instead), but rather is set by a function that chooses between defaultWidth or a wider size depending on the text contained in the vertex.
     */
    public var width: Integer = vertexDefaultWidth;

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
    public var subTreeWidth: Number;
    public var subTreeHeight: Number;
    public var bottomBrink: Number = 0;

    function onClick(e: MouseEvent): Void {
	p("clicked #{index}");
    }

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
				    onMouseClicked: onClick
			    } // Rect
			    , text
		    ] // content
	    } // Group
    } // composeNode

    public function getAllSubNodes(): Vertex[] {
	return [children, for (c in children) c.getAllSubNodes()]
    }

    public function print(): Void {
	    p("{caption} x/y: {x} / {y} P: {parentVertex.index} L: {level} #C: {sizeof children}");
	    p("Width: {width} Height: {height} Index: {index}" );
	    p("subtreeWidth: {subTreeWidth} subTreeHeight: {subTreeHeight}");
    }

	public function toSVG(d :XWDocument): XWElement {
		null
	}
}