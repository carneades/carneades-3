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

import carneadesgui.view.*;
import carneadesgui.GC.*;
import java.lang.System;
import java.lang.Math;

/**
 * A graph layout that displays the argument graph as an ordinary top-down tree.
 */
public class TreeLayout extends GraphLayout {

    function subTreeWidth(v: Vertex): Integer {
		v.subTreeWidth =
				if ((sizeof v.children) > 0) {

					// add up childrens' widths and padding
					var stw = 0;
					for (i in [0 .. sizeof v.children - 1]) {
						stw += subTreeWidth(v.children[i]) + {if (i < sizeof v.children - 1) xPadding else 0}
				}

				// if the children together are less wide than the parent, the parent determines the subtree size.
				if (stw < v.width) v.width else stw;

				} else /*if (v.children == [])*/ {
					// if there are no child nodes, the parent determines the subtree
					v.width;
				}
		v.subTreeWidth as Integer;
    }

    function positionVerticesHorizontally(v: Vertex): Void {
		// the main point here is to see that the root stays at 0,0 and this function positions the children of a node
		if (v.parentVertex == null) {
			// it is the root node
			v.xNew = 0;
			v.yNew = 0;
		}

		var cursor: Number = v.xNew - (v.subTreeWidth / 2);
		for (c in v.children) {
			// position child nodes
			c.xNew = cursor + (c.subTreeWidth / 2);
			cursor = c.xNew + (c.subTreeWidth / 2) + xPadding;
		}

		for (c in v.children) {
			positionVerticesHorizontally(c);
		}
    }

    function positionVerticesVertically(v: Vertex): Void {
		if (v.parentVertex == null) {
			v.yNew = 0;
		} else {
			v.yNew = v.parentVertex.yNew + (v.parentVertex.height / 2) + (v.height / 2) + yPadding;
		}
		for (c in v.children) positionVerticesVertically(c);
    }

    function positionEdges(): Void {
		for (e in graph.edges) {
			e.x1New = e.producer.xNew;
			e.y1New = e.producer.yNew - e.producer.height / 2;
			e.x2New = e.recipient.xNew
				+ {
					// if there is more than one child node, arrange the incoming edges horizontally
					if (sizeof e.recipient.children > 1)
						(- (e.recipient.width / 2)  + e.recipient.bottomBrink +
						(indexOf(e.producer, e.recipient.children)-1) *
						((e.recipient.width - 2*e.recipient.bottomBrink) / (sizeof e.recipient.children - 1)))
					else 0;
				}
			e.y2New = e.recipient.yNew + e.recipient.height / 2;
		}
    }

    override function compose(): Graph {
		// first traversal: determine subtree width
		subTreeWidth(graph.root);

		// second traversal: position vertices horizontally in their subtree
		positionVerticesHorizontally(graph.root);

		// set vertex heights
		positionVerticesVertically(graph.root);

		// layout edges
		positionEdges();

		return graph;
    }
}

