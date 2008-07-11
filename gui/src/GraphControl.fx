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


package Carneades.Control;

// General Imports
import java.lang.System;
import java.lang.Object;
import javafx.animation.*;
import java.io.File;

// Model imports
import Carneades.Argument.*;
import Carneades.Argument.Argument.*;

// View imports
import Carneades.Graph.*;
import Carneades.Graph.GraphList.*;
import Carneades.Graph.Elements.Elements.*;

// Other Control Imports
import Carneades.Control.Commands.*;

public class GraphControl {

	attribute argumentGraph: ArgumentGraph;

	attribute frame: GraphFrame;
	attribute edit: GraphEdit = bind frame.edit;
	attribute view: GraphView = bind frame.view;
	public attribute graph: Graph = bind frame.graph;

	attribute layout: GraphLayout = TreeLayout {
		graph: bind graph
		// todo: this positioning is dirty! Find a better way once hand-dragging is in there.
		width: frame.width - GC.editWidth - 20
		height: frame.height - GC.toolBarHeight - 30
	};

	private attribute commands: CommandControl = CommandControl {
		control: this
	}

	private attribute selectedModels: Object[];

	// View configuration attributes
	public attribute possibleToAddConclusion: Boolean = true;

	public attribute possibleToInverseArgument: Boolean = false;
	public attribute possibleToNegatePremise: Boolean = false;

	public attribute possibleToChangeToOrdPremise: Boolean = false;
	public attribute possibleToChangeToException: Boolean = false;
	public attribute possibleToChangeToAssumption: Boolean = false;

	public attribute possibleToRemove: Boolean = false ;
	public attribute possibleToDelete: Boolean = false ;
	
	public attribute possibleToUndo: Boolean = false;
	public attribute possibleToRedo: Boolean = false;
	
	public attribute dragging: Boolean = false;

	public attribute selectedArgumentEditable: Boolean = false;
	public attribute selectedStatementEditable: Boolean = false;
	public attribute selectedPremiseEditable: Boolean = false;

	public attribute draggingOver = null;

	public attribute currentFile: File = null;
	public attribute fileChanged: Boolean = true;
	public attribute fileLoaded: Boolean = bind currentFile != null;

	public function setDraggingOver(thing): Void { draggingOver = thing; };

	// SELECTION FUNCTIONS

	public attribute possibleToAddArgument = bind selectedModels[0] instanceof Statement;
	public attribute possibleToAddPremise = bind selectedModels[0] instanceof Argument;

	private function singleArgumentLinkSelected(s: GraphElement[]): Boolean {
		return { if (sizeof s != 1) false else (s instanceof ArgumentLink) } 
	}
	private function singlePremiseSelected(s: GraphElement[]): Boolean {
		return { if (sizeof s != 1) false else (s instanceof PremiseLink) } 
	}
	private function premiseSelected(s: GraphElement[]): Boolean {
		return { if (sizeof s != 1) false else (s instanceof PremiseLink) } 
	}
	private function singleSomethingSelected(s: GraphElement[]): Boolean {
		return { (sizeof graph.selected == 1) } 
	}

	public function processSelection(): Void {
		
		// update graph selection
		graph.selected =  [for (v in graph.vertices where v.selected) { v }, for (e in graph.edges where e.selected) { e }];

		if (frame.list.list.selectedItem != null) {
			// 1. The list is selected
			// update model selection
			selectedModels = [];
			insert (frame.list.list.selectedItem as StatementItem).statement into selectedModels;
			// update graph selection
			for (v in graph.vertices) {
				if (v instanceof StatementBox 
					and (v as StatementBox).statement == (frame.list.list.selectedItem as StatementItem).statement) {
						v.selected = true;
						insert v into graph.selected;
					} 
			}
		} else {
			// 2. The graph is selected
			// update model selection
			selectedModels = [];
			for (s in graph.selected) {
				if (s instanceof ArgumentBox) {
					insert (s as ArgumentBox).argument into selectedModels;
				}
				else if (s instanceof ArgumentLink) {
					insert ((s as ArgumentLink).producer as ArgumentBox).argument into selectedModels;
				}
				else if (s instanceof StatementBox) {
					insert (s as StatementBox).statement into selectedModels;
				}
				else if (s instanceof PremiseLink) {
					insert (s as PremiseLink).premise into selectedModels;
				}
			}
		}

		// These calls and their functions can be united into a bind once chained bindings work.
		updateView();
	}
	
	public function unSelectAll(): Void { 
		frame.list.list.selectedItem = null;
		unSelectGraph();
		selectedModels = [];
		updateView();
	}

	public function unSelectGraph(): Void {
		frame.graph.unSelectAll();
		selectedModels = [];
	}

	public function unSelectList(): Void {
		frame.list.list.selectedItem = null;
		selectedModels = [];
	}

	public function getSelected(): Object[] {
		// update Graph selection
		return graph.selected;
	}

	public function getSelectedModel(): Object[] {
		// update Graph selection
		return selectedModels;
	}

	public function selectModel(m: Object) {
		insert m into selectedModels;
	}

	
	// UPDATE FUNCTIONS
	
	private attribute update: Timeline = Timeline {
	// This is the central workaround for the Scenegraph threading problem.
	// Fix this once the do {...} code works again in later versions of the compiler.
    	keyFrames:  KeyFrame {
       		time: 0.01s
       		action: function() {

				// 1. Rendering update
				graph.update();
				layout.compose();

				// 2. Restore Selection
				for (m in selectedModels) {
					// restore arguments
					if (m instanceof Argument) {
						for (v in graph.vertices where (v instanceof ArgumentBox and (v as ArgumentBox).argument == (m as Argument))) {
							v.selected = true;
						}
					}
					// restore statements
					if (m instanceof Statement) {
						for (v in graph.vertices where (v instanceof StatementBox and (v as StatementBox).statement == (m as Statement))) {
							v.selected = true;
						}
					}
					// restore premises
					if (m instanceof Premise) {
						for (v in graph.edges where (v instanceof PremiseLink and (v as PremiseLink).premise == (m as Premise))) {
							v.selected = true;
						}
					}	
					// process it
					processSelection();
				}

				// 3. Update the view
				updateView();

				// close the thread
           		update.stop();

       		} 
    	} 
    	repeatCount: java.lang.Double.POSITIVE_INFINITY
	}

	private function updateView(): Void {
		// function to update the view component influencing booleans
		possibleToUndo = commands.possibleToUndo();
		possibleToRedo = commands.possibleToRedo();
	 	possibleToInverseArgument = singleArgumentLinkSelected(frame.view.graph.selected);
		possibleToRemove = singleSomethingSelected(frame.view.graph.selected);
		var s = frame.list.getSelectedStatement();
		possibleToDelete = { ((s != null) and (not argumentGraph.broughtForth(s))) };
		
		
		if (sizeof getSelectedModel() == 0) {
			frame.list.reset();
		}

		edit.update();
		frame.list.update()
	}

	public function updateAll() {
		update.start();
	}

	public function undo(): Number {
		var result = commands.undo();
		updateAll();
		return result;
	}
	
	public function redo(): Number {
		var result = commands.redo();
		updateAll();
		return result;
	}

	// MODEL MANIPULATION FUNCTIONS

	// DRAGGING FUNCTIONS
	
	override attribute dragging = false;

	public function startDrag(): Void {
		dragging = true;
	}

	public function endDrag(): Void {
		if (dragging) {
			dragging = false;
			// Were we dragging over something?
			if (draggingOver != null) { 
				dragEndsAt(draggingOver as ArgumentElement);
				draggingOver = null;
			}
		}
	}

	public function dragEndsAt(target): Void {
		var selected = getSelected();
		for (s in selected) {
			if (s instanceof StatementBox) {
				if (target instanceof ArgumentBox) {
					// A statement is dragged onto an argument: Append as a premise

					// 1. Determine linking premise
					var temp = (graph.edges[ e | e.producer == s ]);

					var premiseLink: PremiseLink;
					var premise: Premise;

					if (temp != []) {
						premiseLink = temp[0] as PremiseLink;
						premise = premiseLink.premise;
					}

					// If there is no premise (meaning the statement is a root), give it a blank one
					if (premise == null) { premise = Premise { statement: (s as StatementBox).statement }; }

					// 2. Get current parent argument
					var oldArgument: Argument = (premiseLink.recipient as ArgumentBox).argument;

					// 3. Get new parent argument
					var newArgument: Argument = (target as ArgumentBox).argument;

					// 4. issue command
					commands.do(
						MovePremiseCommand {
							argumentGraph: argumentGraph
							premise: premise
							oldArgument: oldArgument
							newArgument: newArgument
						}
					);

					// 5. check for cycles and undo in case
					if (not argumentGraph.noCycles()) { 
						commands.undo();
						commands.pop();
						frame.alert("No! The Graph would become cyclic.");
					}
				}
			}
			if (s instanceof ArgumentBox) {
				if (target instanceof StatementBox) {
					// An argument is dragged over a statement: Append as argument
					
					// 1. Issue the command
					commands.do(
						MoveArgumentCommand {
							argumentGraph: argumentGraph
							argument: (s as ArgumentBox).argument
							oldStatement: (s as ArgumentBox).argument.conclusion
							newStatement: (target as StatementBox).statement
						}
					);
					
					// 2. check for cycles and undo in case
					if (not argumentGraph.noCycles()) { 
						commands.undo(); 
						commands.pop();
						frame.alert("No! The Graph would become cyclic.");
					}
				}
			}
		}
		unSelectAll();
		updateAll();
	}


	public function addStatement(): Void {
		commands.do(
			AddStatementCommand {
				argumentGraph: argumentGraph
			}
		);
		updateAll();
	}

	public function addArgumentToSelected(): Void {
		var selected = getSelectedModel();
		if (sizeof selected > 0) {
			for (s in selected) {
				if (s instanceof Statement) {
					if (commands.do(
							AddArgumentAndPremiseCommand {
								argumentGraph: argumentGraph
								statement: s as Statement
							}
						) != GC.AG_OK) { 
						//frame.alert("Argument cannot be inserted here.\nThe Graph would become cyclic.");
					}
				}
			}
		}

		updateAll();
	}

	public function addPremiseToSelected(): Void {
		var selected = getSelected();
		for (a in selected where a instanceof ArgumentBox) {
			var argument = (a as ArgumentBox).argument;

			commands.do(
				AddPremiseCommand {
					argumentGraph: argumentGraph
					argument: argument
				}
			);
		}

		updateAll();
	}

	// DELETION FUNCTIONS

	public function removeArgumentFromBox(a: ArgumentBox): Void {
		commands.do(
			RemoveArgumentCommand {
				argumentGraph: argumentGraph
				argument: a.argument
			}
		);

		unSelectAll();
		updateAll();
	}

	public function removeArgument(a: Argument): Void {
		commands.do(
			RemoveArgumentCommand {
				argumentGraph: argumentGraph
				argument: a
			}
		);

		unSelectAll();
		updateAll();
	}

	public function removeStatementFromBox(s: StatementBox): Void {
		// get the statement's premise and mother argument if present
		var tempArgument: Argument;
		var tempPremise: Premise;
		for (a in argumentGraph.arguments) {
			for (p in a.premises) {
				if (p.statement == s.statement) {
					tempPremise = p;
					tempArgument = a;
				}
			}
		}
		
		if (argumentGraph.isConclusion(s.statement)) {
		// if the statement is the conclusion of an argument, delete the premise as well as the arguments leading to it
			commands.do(
				DeleteConclusionCommand {
					argumentGraph: argumentGraph
					conclusion: s.statement
					motherArgument: tempArgument
					premise: tempPremise
					childArguments: argumentGraph.arguments[a | a.conclusion == s.statement ]
				}
			);
		} else {
		// if not, delete only the premise
			commands.do(
				DeletePremiseCommand {
					argumentGraph: argumentGraph
					argument: tempArgument
					premise: tempPremise
				}
			);
		}
		
		unSelectAll();
		updateAll();
	}

	public function deleteStatementFromList(): Void {
		var s: Statement = frame.list.getSelectedStatement();
		if (s != null) {
			commands.do(
				DeleteStatementCommand {
					argumentGraph: argumentGraph
					statement: s
				}
			);
		}
		updateView();
	}

	public function removeArgumentFromLink(l: ArgumentLink): Void {
		commands.do(
			RemoveArgumentCommand {
				argumentGraph: argumentGraph
				argument: (l.producer as ArgumentBox).argument
			}
		);

		unSelectAll();
		updateAll();
	}

	public function deletePremiseFromLink(l: PremiseLink): Void {
		for (a in argumentGraph.arguments) {
			for (p in a.premises) {
				if (p == l.premise) {
					commands.do(
						DeletePremiseCommand {
							argumentGraph: argumentGraph
							argument: a
							premise: p
						}
					);
				}
			}
		}
	}

	public function deletePremise(pr: Premise): Void {
		for (a in argumentGraph.arguments) {
			for (p in a.premises) {
				if (p == pr) {
					// todo: bug here: Should be one command for all of them
					commands.do(
						DeletePremiseCommand {
							argumentGraph: argumentGraph
							argument: a
							premise: p
						}
					);
				}
			}
		}
	}

	public function removeSelected(): Void {
		var s = getSelected();
		for (e in s) {
			if (e instanceof ArgumentBox) {
				removeArgumentFromBox(e as ArgumentBox);
			}
			if (e instanceof StatementBox) {
				removeStatementFromBox(e as StatementBox);
			}
			if (e instanceof ArgumentLink) {
				removeArgumentFromLink(e as ArgumentLink);
			}
			if (e instanceof PremiseLink) {
				deletePremiseFromLink(e as PremiseLink);
			}
		}
		unSelectAll();
		updateAll();
	}

	// Attribute Modification Functions

	// for statements

	public function changeStatementId(s: Statement, id: String): Void {
		var admissible: Boolean = true;

		if (not argumentGraph.noDoubleIDs(id)) {
			frame.alert("The chosen id is already taken!");
			admissible = false;
		}

		if (id == "") {
			frame.alert("id may not be empty.");
			admissible = false;
		}

		if (id.matches("^*[:alnum:][:space:]*[:alnum:]$")) {
			frame.alert("id may not contain whitespaces.");
			admissible = false;
		}

		if (admissible) { 
			commands.do(
				ChangeStatementIdCommand {
					argumentGraph: argumentGraph
					statement: s
					id: id
				}
			); 
		}

		updateAll();
	}

	public function changeStatementWff(s: Statement, c: String): Void {
		commands.do(
			ChangeStatementWffCommand {
				argumentGraph: argumentGraph
				statement: s
				wff: c
			}
		);
		updateAll();
	};

	public function changeStatementStatus(s: Statement, v: String): Void {
		commands.do(
			ChangeStatementStatusCommand {
				argumentGraph: argumentGraph
				statement: s
				newStatus: v
			}
		);
		updateAll();
	};

	public function changeStatementProofStandard(s: Statement, st: String, negated: Boolean, complement: Boolean): Void {
			if (st == "SE") {
				commands.do(
					ChangeStatementStandardCommand {
						argumentGraph: argumentGraph
						statement: s
						standard: Scintilla { negated: negated, complement: complement }
					}
				);
			} else if (st == "DV") {
				commands.do(
					ChangeStatementStandardCommand {
						argumentGraph: argumentGraph
						statement: s
						standard: DialecticalValidity { negated: negated, complement: complement }
					}
				);
			} else if (st == "BA") {
				commands.do(
					ChangeStatementStandardCommand {
						argumentGraph: argumentGraph
						statement: s
						standard: BestArgument { negated: negated, complement: complement }
					}
				);
			}
		updateAll();
	};

	// for arguments

	public function changeArgumentDirection(a: Argument, value: String): Void {
		var newValue: Boolean = { if (value == "pro") true else false };
		if (newValue != a.pro) {
			commands.do(
				ChangeArgumentDirectionCommand {
					argumentGraph: argumentGraph
					argument: a
				}
			);
		}

		updateAll();
	}

	public function changeArgumentWeight(a: Argument, v: Number): Void {
		commands.do(
			ChangeArgumentWeightCommand {
				argumentGraph: argumentGraph
				argument: a
				weight: v
			}
		);
		// no new layout needed, so only update the view
		updateView();
	}

	public function changeArgumentScheme(a: Argument, c: String): Void {
		commands.do(
			ChangeArgumentSchemeCommand {
				argumentGraph: argumentGraph
				argument: a
				scheme: c
			}
		);
		updateAll();
	};

	public function changeArgumentId(a: Argument, id: String): Void {
		var admissible: Boolean = true;

		if (not argumentGraph.noDoubleIDs(id)) {
			frame.alert("The chosen id is already taken!");
			admissible = false;
		}

		if (id == "") {
			frame.alert("id may not be empty.");
			admissible = false;
		}
		if (id.matches("^*[:alnum:][:space:]*[:alnum:]$")) {
			frame.alert("id may not contain whitespaces.");
			admissible = false;
		}

		if (admissible) { 
			commands.do(
				ChangeArgumentIdCommand {
					argumentGraph: argumentGraph
					argument: a
					id: id
				}
			);
		}

		updateAll();
	}

	// ... and for premises

	public function negatePremise(p: Premise): Void {
		commands.do(
			NegatePremiseCommand {
				argumentGraph: argumentGraph
				premise: p
			}
		);
		updateAll();
	}

	public function changePremiseType(p: Premise, exception: Boolean): Void {
		if (exception != p.exception) {
			commands.do(
				ChangePremiseTypeCommand {
					argumentGraph: argumentGraph
					premise: p
				}
			);
		}
		updateAll();
	}

	public function changePremiseRole(p: Premise, r: String): Void {
		commands.do(
			ChangePremiseRoleCommand {
				argumentGraph: argumentGraph
				premise: p
				role: r
			}
		);
		updateAll();
	}


	// Load / Save / New Options

	public function newGraph(): Void {
		argumentGraph = GraphControl.defaultGraph();
		
		frame.title = "Carneades";
		currentFile = null;

		commands.reset();
		unSelectAll();
		updateAll();
	}

	public function loadGraphFromFile(f: File): Void {
		// set the current file
		currentFile = f;

		// load the graph
		argumentGraph = ArgumentFile.getGraphFromFile(f);

		frame.title = "Carneades - { f.getAbsolutePath() }";

		commands.reset();
		updateAll();
	}

	public function saveGraphToFile(f: File): Void {
		ArgumentFile.saveGraphToFile(argumentGraph, f);
		currentFile = f;
		frame.title = "Carneades - { f.getAbsolutePath() }";
		fileChanged = false;
	}

	public function saveAsGraphToFile(f: File): Void {

		//Check for overwrite

		ArgumentFile.saveGraphToFile(argumentGraph, f);
		currentFile = f;
		frame.title = "Carneades - { f.getAbsolutePath() }";
		fileChanged = false;
	}

	// DEBUG FUNCTIONS
	public static function defaultGraph(): ArgumentGraph {
		var argumentGraph = ArgumentGraph { id: "TestGraph"};
		
		var s1: Statement = Statement {
			id: "s1"
			wff: "Conclusion"
		}
		
		var s2: Statement = Statement {
			id: "s2"
			wff: "Premise"
		}

		var a1: Argument = Argument {
			id: "a1"
			conclusion: s1
		}

		var p: Premise = Premise {
			statement: s2
		}

		a1.addPremise(p);
		
		argumentGraph.insertStatement(s1);
		argumentGraph.insertStatement(s2);
		argumentGraph.insertArgument(a1);
		
		return argumentGraph;
	}

	// DEBUG PRINT FUNCTIONS

	public function printSelected(): Void {
		System.out.println("vertices: " + graph.selected);
		System.out.println("models: " + graph.selected);
		System.out.println("list:" + frame.list.list.selectedItem);
	}

	public function printSizes(): Void {
		System.out.println("view: " + view.width);
		System.out.println("layout: " + layout.width);
	}

}

