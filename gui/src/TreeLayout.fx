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

import javafx.ext.swing.*;
import javafx.scene.paint.*;
import Carneades.Graph.*;
import Carneades.Graph.GC.*;
import java.lang.System;
import java.lang.Math;

/**
 * A graph layout that displays the argument graph as an ordinary top-down tree.
 */
public class TreeLayout extends GraphLayout {
	var root: Vertex;
	
	// helper function for debugging
	public function debug(s: String):Void {
		System.out.println(s);
	}

	// helper function to be discardad once select code works
	 function pickChild(parentVertex: Vertex, priority: Number):Vertex {
		var found: Vertex;
		for (i in parentVertex.children) {
			if (i.priority == priority) { found = i; }
		}
		return found;
	}

	 function setPriorities(v: Vertex):Void {
		// Set child node drawing priorities
		// right now: Simple child array index duplication
		if (v.children != null) {
			for (i in [0 .. (sizeof v.children-1)]) {
				v.children[i].priority = i;
				setPriorities(v.children[i]);
			}
		}
	} 

	 function getBottomLeft(v: Vertex):Vertex {
		// to do: check for empty graph
		var bottomLeft: Vertex = v;
		while (bottomLeft.children != null ) {
			var next: Vertex;
			for	(i in bottomLeft.children) {
				if (i.priority == 0) {
					next = i;
				}
			}
			bottomLeft = next;
		}
		return bottomLeft;
	}
	
	 function treeSize(v: Vertex):Void {
		treeSize(v, false);
	}

	// FIRST TRAVERSAL: Determine Subtree Sizes
	// parameter is the "BOTTOM LEFT" node of the tree
	 function treeSize(v: Vertex, complete: Boolean):Void {
		
		if (d) debug("sizing {v.caption}");
		if (v.children == null) { // It is a leaf node
			if (d) debug("... it is a leaf node");

			// determine size if the subtree as the size of the node
			v.xSubTreeSize = v.width;

			// do the next sibling if there are any left
			if ((sizeof v.parentVertex.children) > (v.priority + 1)) {
				if (d) debug("Attempting to size sibling");
				// take the sibling with the next priority
				treeSize(pickChild(v.parentVertex, v.priority + 1))
			
			} else {
				if (d) debug("No sibling found ...");
				// else do the parent if there is one
				if (v.parentVertex != null) {
					if (d) debug("Attempting to size parent");
					treeSize(v.parentVertex, true);
				}
			}

		} else { // It is no leaf node
			if (d) debug("... it is no leaf node");
			
			// Is the subtree already complete?
			if (complete) {
				if (d) debug("... the subtree is complete");
				// determine the subtree size
				v.xSubTreeSize = 0;
				for (i in v.children) {
					// add every subtree's size
					v.xSubTreeSize += i.xSubTreeSize;
				}
				// add spacing in between
				v.xSubTreeSize += (sizeof v.children - 1) * xDistance;

				// check whether the node is wider than its subtree
				if (v.width > v.xSubTreeSize) {
					v.xSubTreeSize = v.width;
				}

				// do the next sibling if there are any left
				if ((sizeof v.parentVertex.children) > (v.priority + 1)) {
					if (d) debug("Attempting to size sibling");
					// take the sibling with the next priority
					treeSize(pickChild(v.parentVertex, v.priority+1))
				
				} else {
					if (d) debug("No sibling found ...");
					// else do the parent if there is one
					if (v.parentVertex != null) {
						if (d) debug("Attempting to size parent");
						treeSize(v.parentVertex, true);
					}
				}

			} else {
				// get new bottom left and layout it
				treeSize(getBottomLeft(v));
			}
		}
	}


	// SECOND TRAVERSAL: Assign horizontal positions
	// parameter is the ROOT node of the tree
	 function position(v: Vertex):Void {
		
		// determine whether the parent is wider than all its child subtrees together
		var childSum: Number = 0;
		for (c in v.children) { childSum += c.xSubTreeSize; } // add up children
		childSum += xDistance * (sizeof v.children - 1); // add gaps
		var parentWider: Boolean = (v.xSubTreeSize > childSum);
		
		if (sizeof v.children > 1) {
			// if there is more than one child, position them one at a time using a "cursor"
			// Layout children of passed node
			
			if (not parentWider) {
				var cursor: Number = (- v.xSubTreeSize / 2) + (pickChild(v, 0).xSubTreeSize / 2);
		
				for (i in [0 .. (sizeof v.children-1)]) {
					var current: Vertex = pickChild(v, i);
					current.xShift = cursor;			
					cursor += (current.xSubTreeSize / 2) + xDistance;
	
					if (i < (sizeof v.children-1)) cursor += (pickChild(v, i+1).xSubTreeSize / 2);
				}
			} else {
				var cursor: Number = (- childSum / 2) + (pickChild(v, 0).xSubTreeSize / 2);
		
				for (i in [0 .. (sizeof v.children-1)]) {
					var current: Vertex = pickChild(v, i);
					current.xShift = cursor;			
					cursor += (current.xSubTreeSize / 2) + xDistance;
	
					if (i < (sizeof v.children-1)) cursor += (pickChild(v, i+1).xSubTreeSize / 2);
				}				
			}

			// position the children's children recursively
			for (i in v.children) position(i);

		} else {
			// if there is only one child, position it straight below
			if (sizeof v.children == 1) {
				v.children[0].xShift = 0;
				position(v.children[0]);
			}
		}
	}


	 function adjust():Void {
		// set the overall tree width
		this.width = Math.max(appWidth - editWidth - 30, root.xSubTreeSize).intValue();

		// adjust vertical alignment
		for (i:Vertex in graph.vertices where i.level > 0) {
			i.yShift = (i.height/2) + (i.parentVertex.height / 2) + yDistance;
		}

		// find deepest node
		var bottom: Number = 0;
		for (i: Vertex in graph.vertices) {
			bottom = Math.max(bottom, i.y - graph.root.yShift);
		}

		// adjust overall tree height
		this.height = Math.max( appHeight - toolBarHeight - 50 , bottom).intValue();
	}

	 function layoutEdges():Void {
		// adjust edges

		// 1. determine direction from which edge is coming in
		for ( i in graph.edges) {
			if (i.producer.level < i.recipient.level) {
				// edge comes top down
				i.direction = TOP;
			} else {
				// edge comes bottom up
				i.direction = BOTTOM;
			}
		}


		// 2. move line ends to the vertically correct points according to the levels they span
		for ( i in graph.edges) {
			if (i.producer.level < i.recipient.level) {
				// edge comes top down
				i.y1Shift = i.producer.height;
			} else {
				// edge comes bottom up
				i.y2Shift = i.recipient.height / 2;
				i.y1Shift = - i.producer.height / 2;
			}
		}

		// 3. move line ends to correct horizontal position according to the box end
		for ( i in graph.edges) {
			if (i.producer.level < i.recipient.level) {
				// edge comes top down
				i.x2Shift = - (i.producer.width / 2) // move to lower left corner of the bottom edge
							+ (i.producer.width / (sizeof i.producer.children + 1)) * (i.recipient.priority + 1);

			} else {
				// edge comes bottom up
				i.x2Shift = - ((i.recipient.width-i.recipient.bottomBrink) / 2) // move to lower left corner of the bottom edge
							+ ((i.recipient.width-i.recipient.bottomBrink) / (sizeof i.recipient.children + 1)) * (i.producer.priority + 1);
			}
		}

		// set head angles
	}


	override function compose():Graph {
		//System.out.println("layout started!");

		// update box widths
		// This is a very poor workaround, but I see no way to trigger an update event in here...
		for (v in graph.vertices) {
			if (v.scaleWithText) { 
				v.width = Math.max(50, v.text.getWidth() + 10);
			} else {
				v.width = v.defaultWidth;
			}
		}

		var roots: Vertex[];
					
		// get roots
		for (v in graph.vertices) {
			if (v.parentVertex == null) { insert v into roots; }
		}

		root = roots [0];
			
		// set graph priorities for drawing the nodes in order
		setPriorities(root);
			
		// do the main tree vertex layout
		// First Traversal: determine tree sizes
		treeSize(getBottomLeft(root));
		position(root);
			
		// adjust layout according to the offset
		adjust();
						
		layoutEdges();
				
		return graph;
	}
}

