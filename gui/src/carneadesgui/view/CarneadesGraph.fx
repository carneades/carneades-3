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

// import the rest of the graph package
import carneadesgui.GC.*;
import carneadesgui.view.*;
import carneadesgui.view.Elements.*;

// import the necessary parts of the model
import carneadesgui.model.*;
import carneadesgui.model.Argument.*;

// control import
import carneadesgui.control.CarneadesControl;
import carneadesgui.control.Commands.*;

import java.lang.System;

/**
 * The view graph class displaying a model graph object.
 */
public class CarneadesGraph extends Graph {

    public var control: CarneadesControl;

    /**
     * The command control and stack object associated with the graph. Each graph has its own commands object so that each graph can have its own undo history.
     */
    public var commands: CommandControl = CommandControl {
		control: control
    }

    /**
     * The model graph object displayed through the CarneadesGraph object.
     */
    public var argumentGraph: ArgumentGraph;

    // the invisible root node
    override var root = CarneadesVertex {
		caption: "root"
		x: 0
		y: 0
		visible: false
		toBeDisplayed: false
    }

    function getStatementBox(s: Statement): StatementBox {
		// is it in the graph?
		var ab: StatementBox;
		if ({ // if it is
			var found: Boolean = false;
			for (v in vertices)
				if (v instanceof StatementBox)
					if ((v as StatementBox).statement == s and not (v as StatementBox).duplicate) {
						ab = v as StatementBox;
						found = true;
					}
			found
		}) ab else null
    }

    function getArgumentBox(a: Argument): ArgumentBox {
		// is it in the graph?
		var ab: ArgumentBox;
		if ({ // if it is
			var found: Boolean = false;
			for (v in vertices)
			if (v instanceof ArgumentBox)
				if ((v as ArgumentBox).argument == a) {
					ab = v as ArgumentBox;
					found = true;
				}
			found
		}) ab else null
    }

    function getArgumentLink(a: Argument): ArgumentLink {
		// is it in the graph?
		var al: ArgumentLink;
		if ({ // if it is
			var found: Boolean = false;
			for (e in edges)
				if (e instanceof ArgumentLink)
					if ((e as ArgumentLink).argument == a) {
						al = e as ArgumentLink;
						found = true;
					}
				found
		}) al else null
    }

    function getPremiseLink(p: Premise): PremiseLink {
		// is it in the graph?
		var ab: PremiseLink;
		if ({ // if it is
			var found: Boolean = false;
			for (e in edges)
				if (e instanceof PremiseLink)
					if ((e as PremiseLink).premise == p) {
						ab = e as PremiseLink;
						found = true;
					}
				found
		}) ab else null
    }

	function update_rec(element: Vertex): Void {
		var argumentBoxes: ArgumentBox[] = [for (v in vertices where v instanceof ArgumentBox) v as ArgumentBox];
		var argumentLinks: ArgumentLink[] = [for (e in edges where e instanceof ArgumentLink) e as ArgumentLink];
		var premiseLinks: PremiseLink[] = [for (e in edges where e instanceof PremiseLink) e as PremiseLink];

		var putPremiseLinkIfNeeded = function(p: Premise, abox: ArgumentBox, sbox: StatementBox): Void {
			var isAlreadyThere: Boolean = false;
			for (pl in premiseLinks where pl.recipient == abox and pl.producer == sbox) {
				isAlreadyThere = true;
				pl.updated = true;
			}
			if (not isAlreadyThere) {
				insert PremiseLink {
					control: control
					premise: p
					producer: sbox
					recipient: abox
					updated: true
				} into edges;
			}
		}

		var putArgumentLinkIfNeeded = function(abox: ArgumentBox, sbox: StatementBox): Void {
			var isAlreadyThere: Boolean = false;
			for (al in argumentLinks where al.recipient == sbox and al.producer == abox) {
				isAlreadyThere = true;
				al.updated = true;
			}
			if (not isAlreadyThere) {
				insert ArgumentLink {
					control: control
					argument: abox.argument
					producer: abox
					recipient: sbox
					updated: true
				} into edges;
			}
		}

		// STATEMENT BOXES
		if (element instanceof StatementBox) {
			var sbox: StatementBox = element as StatementBox;
			// get the arguments leading to it
			for (a in argumentGraph.arguments where a.conclusion == sbox.statement) {
				// check if there is an argument box for it
				var boxAlreadyThere: Boolean = false;
				for (c in argumentBoxes) {
					// if there is one ...
					if ((c as ArgumentBox).argument == a) {
						boxAlreadyThere = true;
						c.updated = true;
						delete c from c.parentVertex.children;
						c.parentVertex = sbox;
						insert c into sbox.children;

						// put in argument link if necessary
						putArgumentLinkIfNeeded(c as ArgumentBox, sbox);

						// do the recursive call if necessary
						if (a.premises != []) update_rec(c as ArgumentBox);
					}
				}
				if (not boxAlreadyThere) {
					// if there is none ...
					var abox: ArgumentBox = ArgumentBox {
						argument: a
						control: control
						updated: true
						parentVertex: sbox
					}
					insert abox into vertices;
					insert abox into sbox.children;

					// put in argument link if necessary
					putArgumentLinkIfNeeded(abox, sbox);

					// do the recursive call if necessary
					if (a.premises != []) update_rec(abox);
				}
			}

		// ARGUMENT BOXES
		} else if (element instanceof ArgumentBox) {
			var abox: ArgumentBox = element as ArgumentBox;
			for (p in abox.argument.premises) {
				// check if we have a statement box
				var boxAlreadyThere: Boolean = false;
				for (c in abox.children) {
					// if there is one ...
					if ((c as StatementBox).statement == p.statement) {
						boxAlreadyThere = true;
						c.updated = true;
						delete c from c.parentVertex.children;
						c.parentVertex = abox;
						insert c into abox.children;
						(c as StatementBox).duplicate = {
							// if there is some statement box with the same statement, this one must be a duplicate
							var anotherOne: Boolean = false;

							for (v in vertices where v instanceof StatementBox)
								// The other one ...
								// ... has to be a statement box representing the same statement
								if ((v as StatementBox).statement == p.statement
									// .. that is distinct ...
									and v != c
									// and not a duplicate itself
									and not (v as StatementBox).duplicate
									and (
											// and it is either
											(
												// somewhere in the tree as a premise ...
												(v as StatementBox).parentVertex != root
												and isMemberOf(((v as StatementBox).parentVertex as ArgumentBox).argument,
												(v as StatementBox).statement.arguments)
											)
											or (
												// or legitimately at the tree root
												(v as StatementBox).parentVertex == root
												and isMemberOf((v as StatementBox).statement, argumentGraph.statements)
											)
										)
									) anotherOne = true;
							anotherOne
						}

						// put in premise link if necessary
						putPremiseLinkIfNeeded(p, abox, c as StatementBox);
						
						// do rec. call
						if (not (c as StatementBox).duplicate) update_rec(c as StatementBox);
					}
				}
				if (not boxAlreadyThere) {
					// if there is none ...
					var sbox: StatementBox = StatementBox {
						statement: p.statement
						control: control
						updated: true
						parentVertex: abox
						duplicate: {
							// if there is some statement box with the same statement, this one must be a duplicate
							var anotherOne: Boolean = false;
							for (v in vertices where v instanceof StatementBox)
								if ((v as StatementBox).statement == p.statement
									and not (v as StatementBox).duplicate) anotherOne = true;
							anotherOne
						}
					}
					insert sbox into vertices;
					insert sbox into abox.children;

					// put in premise link if necessary
					putPremiseLinkIfNeeded(p, abox, sbox);
					
					// do rec. call
					if (not sbox.duplicate) update_rec(sbox);
				}
			}

		} else {
			p("update_rec() has been called on a non-box vertex. Debug.")
			// should not
		}

	}

	override function updateFromModel(): Void {

		// tell the graph elements that they have not been traversed
		for (e in [vertices, edges]) e.updated = false;

		// do a preliminary cleaning of deleted elements
		var statementBoxes: StatementBox[] = [for (v in vertices where v instanceof StatementBox) v as StatementBox];
		var argumentBoxes: ArgumentBox[] = [for (v in vertices where v instanceof ArgumentBox) v as ArgumentBox];

		for (sbox in statementBoxes where not isMemberOf(sbox.statement, argumentGraph.statements))
			deleteVertexAndEdges(sbox);

		for (sbox in statementBoxes where sbox.parentVertex == root and sbox.statement.arguments != [])
			deleteVertexAndEdges(sbox);

		for (abox in argumentBoxes where not isMemberOf(abox.argument, argumentGraph.arguments))
			deleteVertexAndEdges(abox);

		// create/update all statement boxes that are children of the root
		for (s in argumentGraph.statements where s.arguments == []) {
			// check whether it has a statement box
			var sbox: StatementBox = [ for (sb in vertices where (sb instanceof StatementBox)
				and (sb as StatementBox).statement == s and sb.parentVertex == root) sb as StatementBox][0];
			if (sbox == null) {
				// if there is no box, create one
				sbox = StatementBox {
					statement: s
					parentVertex: root
					control: control
					updated: true
				}
				insert sbox into vertices;
				insert sbox into root.children;

				// and call recursively for its children
				if (argumentGraph.isConclusion(s)) update_rec(sbox);
			} else {
				// if there already is a box, do the subtree
				sbox.updated = true;
				if (argumentGraph.isConclusion(s) and not sbox.duplicate) update_rec(sbox);
			}
		}

		// the root is updated
		root.updated = true;

		// Clean up
		for (v in vertices where not v.updated) {
			v.children = [];
			delete v from v.parentVertex.children;
			v.parentVertex = null;
			delete v from vertices;
		}

		for (e in edges where not e.updated) {
			e.producer = null;
			e.recipient = null;
			delete e from edges;
		}
	}
	
    

    public function printGraph(): Void {
	    for (v in vertices) v.print();
    }

}
