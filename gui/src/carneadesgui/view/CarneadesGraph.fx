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
import carneadesgui.control.Commands.CommandControl;
import carneadesgui.control.CarneadesControl;

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
					if ((v as StatementBox).statement == s) {
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

    function updateGraphConnections(): Void {
		var statementBoxes: StatementBox[] = [for (v in vertices where v instanceof StatementBox) v as StatementBox];
		var argumentBoxes: ArgumentBox[] = [for (v in vertices where v instanceof ArgumentBox) v as ArgumentBox];

		// delete all connections
		for (v in vertices) v.children = [];

		// determine parent/child and set it
		// for statements
		for (s in statementBoxes) {

			var found: Vertex[];
			for (a in argumentBoxes){
				for (p in a.argument.premises) {
					if (p.statement == s.statement) {
						insert a into found;
						insert s into a.children
					}
				}
			}
			if (sizeof found > 0) {
				s.parentVertex = found [0];
			} else {
				s.parentVertex = root;
				insert s into root.children;
			}
		}

		// for arguments
		for (a in argumentBoxes) {
			var found: Vertex[];
			for (s in statementBoxes) {
				if (a.argument.conclusion == s.statement) {
					insert s into found;
					insert a into s.children;
				}
			}
			if (sizeof found > 0) {
				a.parentVertex = found [0];
			} else if (a != root) {
				a.parentVertex = root;
				insert a into root.children;
			}
		}
    }

    function createStatementBox(s: Statement): StatementBox {
		var statementBox: StatementBox = StatementBox {
			statement: s
			level: bind { statementBox.parentVertex.level + 1}
			control: control
		}
    }

    function createArgumentBox(a: Argument): ArgumentBox {
		var argumentBox: ArgumentBox = ArgumentBox {
			argument: a
			level: bind { argumentBox.parentVertex.level + 1}
			control: bind control
		}
    }

    override function updateFromModel(): Void {

		// VERTICES
		// if there is no graph create the whole thing
		if (vertices == []) {
			root.children = [];
			vertices = [root, toVertices(argumentGraph.statements, argumentGraph.arguments)];
		} else {
			// if there is something we assume an alteration and hence update accordingly

			// has a statement been added?
			for (s in argumentGraph.statements) {
				if (getStatementBox(s) == null) {
					insert createStatementBox(s) into vertices;
				}
			}

			// has an argument been added?
			for (a in argumentGraph.arguments) {
				// is it in the graph?
				var ab: ArgumentBox = getArgumentBox(a);
				if (ab != null) {
					// check whether it still connects to all its premises
					for (p in a.premises) {
						var pl: PremiseLink = getPremiseLink(p);
						if (pl == null) {
							insert PremiseLink {
								premise: p
								producer: getStatementBox(p.statement)
								recipient: getArgumentBox(a)
								negated: bind p.negative
								control: bind control
							} into edges;
						} else {
							if (pl.recipient != ab) pl.recipient = ab;
						}
					}

				} else {
					// if not, add it and connect it according
					insert createArgumentBox(a) into vertices;
					insert ArgumentLink {
						argument: a
						recipient: getStatementBox(a.conclusion)
						producer: getArgumentBox(a)
						control: bind control
					} into edges;

					for (p in a.premises)
					if (getPremiseLink(p) == null) {
						insert PremiseLink {
							premise: p
							producer: getStatementBox(p.statement)
							recipient: getArgumentBox(a)
							negated: bind p.negative
							control: bind control
						} into edges;
					}
				}

				// is the link proper?
				if (getArgumentLink(a) != null) {
					var al: ArgumentLink = getArgumentLink(a);
					if (((al.recipient) as StatementBox).statement != a.conclusion) {
						al.recipient = getStatementBox(a.conclusion);
					}
				}
			}

			// has stuff been been removed?
			var statementBoxes: StatementBox[] = [for (v in vertices where v instanceof StatementBox) v as StatementBox];
			var argumentBoxes: ArgumentBox[] = [for (v in vertices where v instanceof ArgumentBox) v as ArgumentBox];
			var argumentLinks: ArgumentLink[] = [for (e in edges where e instanceof ArgumentLink) e as ArgumentLink];
			var premiseLinks: PremiseLink[] = [for (e in edges where e instanceof PremiseLink) e as PremiseLink];
			for (ab in argumentBoxes)
				if (not isMemberOf(ab.argument, argumentGraph.arguments))
					removeWithFade(ab);
			for (al in argumentLinks)
				if (not isMemberOf(al.argument, argumentGraph.arguments))
					removeWithFade(al);
			for (sb in statementBoxes)
				if (not isMemberOf(sb.statement, argumentGraph.statements))
					removeWithFade(sb);
			for (pl in premiseLinks)
				if ({
					var obsolete: Boolean = true;
					for (a in argumentGraph.arguments)
						if (isMemberOf(pl.premise, a.premises))
							obsolete = false;
					obsolete
				}) removeWithFade(pl);

			// has an argument been moved?
		}

		// EDGES
		updateGraphConnections();
		if (edges == []) {
			edges = toEdges(argumentGraph.statements, argumentGraph.arguments);
		}
    }

    function toVertices(statements: Statement[], arguments: Argument[]): Vertex[] {
		var statementBoxes: StatementBox[];
		var argumentBoxes: ArgumentBox[];
		// 1. create bound vertices
		// for statements
		for (s in statements) {
			if (drawAllStatements or (argumentGraph.broughtForth(s) or argumentGraph.isPremise(s))) {
				insert createStatementBox(s) into statementBoxes;
			}
		}
		// for arguments
		for (a in arguments) {
			insert createArgumentBox(a) into argumentBoxes;
		}

		return [argumentBoxes, statementBoxes];
    }

    function toEdges(statements: Statement[], arguments: Argument[]): Edge[] {
		var links: Edge[];

		// 1. argument links
		for (a in arguments where (a.conclusion != null)) {
			var link: Edge;
			var producer: ArgumentBox[] = (for (v in vertices where
				((v instanceof ArgumentBox)
				and (v as ArgumentBox).argument == a))
					{ v as ArgumentBox});
			var recipient: StatementBox[] = (for (v in vertices where
				((v instanceof StatementBox)
				and (v as StatementBox).statement == a.conclusion))
					{ v as StatementBox });

			link = ArgumentLink {
				argument: a
				producer: producer[0]
				recipient: recipient[0]
				control: bind control
			}
			insert link into links;
		}

		// 2. Premises
		for (a in arguments) {
			for (p in a.premises) {
				var link: Edge;
				var recipient: ArgumentBox[] = (for (v in vertices where
					((v instanceof ArgumentBox)
					and (v as ArgumentBox).argument == a))
						{ v as ArgumentBox});
				var producer: StatementBox[] = (for (v in vertices where
					((v instanceof StatementBox)
					and (v as StatementBox).statement == p.statement))
						{ v as StatementBox });
				link = PremiseLink {
					premise: p
					producer: producer[0]
					recipient: recipient[0]
					negated: bind p.negative
					control: bind control
				}
				insert link into links;
			}
		}
		return links;
    }

    public function printGraph(): Void {
	    for (v in vertices) v.print();
    }

}
