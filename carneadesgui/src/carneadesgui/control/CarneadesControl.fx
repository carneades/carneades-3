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

package carneadesgui.control;

// General Imports
import java.io.File;
import carneadesgui.GC.*;

// Model imports
import carneadesgui.model.*;
import carneadesgui.model.Argument.*;

// View imports
import carneadesgui.view.*;
import carneadesgui.view.Elements.*;
import carneadesgui.view.TreeLayout;
import carneadesgui.view.GraphUpdate;

// Other Control Imports
import carneadesgui.control.Commands.*;

import javafx.animation.KeyFrame;
import javafx.animation.Timeline;

// File Chooser for Load/Save
import javax.swing.JFileChooser;
import javax.swing.JDialog;
import javax.swing.JOptionPane;

/**
 * Central control class for the Carneades application. It instantiates the needed view and model objects
 */

public class CarneadesControl {

	/**
	* The application's model component. Should be set using setModel() in the application object post-init.
	*/
	public var model: CarneadesModel = null;

	/**
	 * The application's view. Should be set using setView() from the CarneadesGUI object post-init.
	 */
	public var view: CarneadesView = null;

	/**
	 * The array of currently loaded model argument graphs in the model component. Read-Only.
	 */
	public var argumentGraphs: ArgumentGraph[] = model.argumentGraphs;

	/**
	 * The current model argument graph that is to be displayed. Read-Only.
	 */
	public var argumentGraph: ArgumentGraph = bind graph.argumentGraph;

	/**
	 * The application's inspector panel.
	 */
	//public var edit: GraphEdit = bind frame.edit;
	
	/**
	 * The array of graph objects that correspond to the model graphs.
	 */
	public var graphs: CarneadesGraph[] = [];

	/**
	 * The view graph object currently displayed.
	 */
	public var graph: CarneadesGraph;

	/**
	 * The command administering unit of the currently displayed graph.
	 */
	var commands: CommandControl = bind graph.commands;

	// !!! -> This should be moved into the graph class!
	// var selectedModels: Object[];

	// View configuration attributes
	public var possibleToAddConclusion: Boolean = true;

	public var possibleToInverseArgument: Boolean = false;
	public var possibleToNegatePremise: Boolean = false;

	public var possibleToChangeToOrdPremise: Boolean = false;
	public var possibleToChangeToException: Boolean = false;
	public var possibleToChangeToAssumption: Boolean = false;

	public var possibleToRemove: Boolean = false ;

	public var possibleToUndo: Boolean = false;
	public var possibleToRedo: Boolean = false;

	public var dragView: Boolean = false;

	public var selectedArgumentEditable: Boolean = false;
	public var selectedStatementEditable: Boolean = false;
	public var selectedPremiseEditable: Boolean = false;

	/**
	 * Is the user just dragging something over something else?
	 */
	public var draggingOver: Object = false;

	public var currentFile: File = null;
	public var fileChanged: Boolean = true;
	public var fileLoaded: Boolean = bind currentFile != null;
	public var fileChooser: JFileChooser = new JFileChooser();


	public function setView(view: CarneadesView) {
		view.control = this;
		this.view = view;
	}

	public function setModel(view: CarneadesModel) {
		this.model = model;
	}

	/**
	 * Set the object the users currently drags something over. Should be called by all view obects receiving dragging actions in their onMouseEnter/onMouseLeave methods.
	 */
	public function setDraggingOver(thing): Void { draggingOver = thing; };

	// SELECTION FUNCTIONS

	public var possibleToAddArgument = bind graph.selectedModels[0] instanceof Statement;
	public var possibleToAddPremise = bind graph.selectedModels[0] instanceof Argument;

	function singleStatementSelected(): Boolean {
		return graph.selectedModels[0] instanceof Statement
	}
	function singleArgumentSelected(): Boolean {
		return graph.selectedModels[0] instanceof Argument
	}
	function singlePremiseSelected(): Boolean {
		var s: GraphElement[] = graph.selectedElements();
		return { if (sizeof s != 1) false else (s[0] instanceof PremiseLink) }
	}
	function premiseSelected(): Boolean {
		var s: GraphElement[] = graph.selectedElements();
		return { if (sizeof s != 1) false else (s[0] instanceof PremiseLink) }
	}
	function singleSomethingSelected(): Boolean {
		return { (sizeof graph.selectedElements() == 1) }
	}
	function nothingSelected(): Boolean {
		return (sizeof graph.selectedElements() == 0);
	}

	public function processGraphSelection(g: GraphElement): Void {
		unSelectAll();
		g.selected = true;
		graph.updateSelectedModelsFromElements();
		focusOnSelected();
		updateView( GraphUpdate { graphSelection: true } );
	}
	
	public function processListSelection(e: Object): Void {
		unSelectGraph();
		if (not (e instanceof ArgumentGraph))
		{
			insert e into graph.selectedModels;
			graph.updateSelectedElementsFromModel();
			focusOnSelected();
			updateView( GraphUpdate { listSelection: true } );
		} else {
			view.editGraph(e as ArgumentGraph);
			if ((e as ArgumentGraph) != argumentGraph) displayGraph(e as ArgumentGraph);
		}
	}

	/**
	 * Unselects view and lists.
	 */
	public function unSelectAll(): Void {
		unSelectGraph();
		view.unSelectAll();
		view.mode = inspectorDefaultMode;
		updateView( GraphUpdate { selection: true} );
	}

	/**
	 * Unselects the graph view only.
	 */
	public function unSelectGraph(): Void {
		graph.unSelectAll();
	}

	/**
	 * Focus the view on a certain view object.
	 */
	public function focusOnElement(e: GraphElement) {
		view.focusOn(e);
	}

	/**
	 * Focus the view on the selected view object.
	 */
	public function focusOnSelected() {
		view.focusOn(graph.selectedElements()[0]);
	}

	/**
	 * Switch the view to another argument graph.
	 */
	public function displayGraph(a: ArgumentGraph) {
		graph = (for (g in graphs where (g as CarneadesGraph).argumentGraph == a) { g }) [0];
		updateAll();
	}

	// UPDATE FUNCTIONS
	// TODO clean this up. This timeline belongs into the view.
	var update: Timeline = Timeline {
		repeatCount: 1
    	keyFrames:  KeyFrame {
       		time: 0.01s
       		action: function() {

				// 1. Rendering update
				graph.update();
				graph.glayout.compose();

				// 2. Restore Selection
				graph.updateSelectedElementsFromModel();
				
       		}
    	}
	}

	function updateView(u: GraphUpdate): Void {
		if (u.layout) update.playFromStart();

		// set modes
		if (singleStatementSelected()) view.editStatement(graph.selectedModels[0] as Statement);
		if (singleArgumentSelected()) view.editArgument(graph.selectedModels[0] as Argument);
		if (singlePremiseSelected()) view.editPremise(graph.selectedModels[0] as Premise);
		if (nothingSelected()) view.editNothing();

		// update the view component influencing booleans
		possibleToUndo = commands.possibleToUndo();
		possibleToRedo = commands.possibleToRedo();
	 	possibleToInverseArgument = singleArgumentSelected();
		possibleToRemove = singleSomethingSelected();

		view.update(u);
	}

	/**
	 * Do a global view and controls update.
	 */
	public function updateAll() {
		updateView( GraphUpdate {
			layout: true
			selection: true
		});
	}

	/**
	 * Perform an undo action.
	 */
	public function undo(): Number {
		var result = commands.undo();
		updateAll();
		return result;
	}

	/**
	 * Perform a redo action.
	 */
	public function redo(): Number {
		var result = commands.redo();
		updateAll();
		return result;
	}

	// MODEL MANIPULATION FUNCTIONS

	// DRAGGING FUNCTIONS

	/**
	 * Is a dragging action in progress.
	 */
	public var dragging = false;

	/**
	 * Processes the start of a dragging action. Should be called whenever a dragging action starts.
	 */
	public function startDrag(): Void {
		dragging = true;
	}

	/**
	 * Processes the end of a dragging action. Should be called when a dragging action ends.
	 */
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

	 function dragEndsAt(target): Void {
		var selected = graph.selectedElements();
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
						view.alert("No! The Graph would become cyclic.");
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
						view.alert("No! The Graph would become cyclic.");
					}
				}
			}
		}
		unSelectAll();
		updateAll();
	}

	/**
	 * Adds a blank statement to the graph.
	 */
	public function addStatement(): Void {
		commands.do(
			AddStatementCommand {
				argumentGraph: argumentGraph
			}
		);
		updateAll();
	}

	/**
	 * Append an argument to the selected statement.
	 */
	public function addArgumentToSelected(): Void {
		var selected = graph.selectedModels;
		if (sizeof selected > 0) {
			for (s in selected) {
				if (s instanceof Statement) {
					if (commands.do(
							AddArgumentAndPremiseCommand {
								argumentGraph: argumentGraph
								statement: s as Statement
							}
						) != C_OK) {
						view.alert("Argument cannot be inserted here.\nThe Graph would become cyclic.");
					} else {
						updateAll();
					}
				}
			}
		}
	}

	/**
	 * Append a premise to the selected argument.
	 */
	public function addPremiseToSelected(): Void {
		var selected = graph.selectedElements();
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
	/**
	 * Removes an argument from a marked view argument node.
	 */
	public function removeArgumentFromBox(ar: Argument): Void {
		var a: Argument = null;
		if (ar != null) a = ar else a = graph.selectedModels[0] as Argument;

		commands.do(
			RemoveArgumentCommand {
				argumentGraph: argumentGraph
				argument: a
			}
		);

		unSelectAll();
		updateAll();
	}

	/**
	 * Removes an argument from a selected model argument (e.g. off a list).
	 */
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

	/**
	 * Remove a statement from whose view object has been selected.
	 */
	public function removeStatementFromBox(st: Statement): Void {
		
		var s: Statement = null;
		if (st != null) s = st else s = graph.selectedModels[0] as Statement;

		// get the statement's premise and mother argument if present
		var tempArgument: Argument;
		var tempPremise: Premise;
		for (a in argumentGraph.arguments) {
			for (p in a.premises) {
				if (p.statement == s) {
					tempPremise = p;
					tempArgument = a;
				}
			}
		}

		if (argumentGraph.isConclusion(s)) {
		// If the statement is the conclusion of an argument,
		// delete the premise as well as the arguments leading to it.
			commands.do(
				DeleteConclusionCommand {
					argumentGraph: argumentGraph
					conclusion: s
					motherArgument: tempArgument
					premise: tempPremise
					childArguments: argumentGraph.arguments[a | a.conclusion == s ]
				}
			);
		} else if (argumentGraph.isPremise(s)){
		// If it is a premise, delete both statement and premise.
			commands.do(
				DeletePremiseStatementCommand {
					argumentGraph: argumentGraph
					argument: tempArgument
					premise: tempPremise
				}
			);
		} else {
		// Otherwise, delete the statement only.
			commands.do(
				DeleteStatementCommand {
					argumentGraph: argumentGraph
					statement: s
				}
			);
		}

		unSelectAll();
		updateAll();
	}

	/**
	 * Remove a statement that has been selected in the statement list.
	 */
	/*p
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
		//p unSelectAll();
		updateAll();
		//p updateView();
	}*/


	/**
	 * Remove an argument from its selected link.
	 */
	public function removeArgumentFromLink(l: ArgumentLink): Void {
		commands.do(
			RemoveArgumentCommand {
				argumentGraph: argumentGraph
				argument: (l.producer as ArgumentBox).argument
			}
		);

		//p unSelectAll();
		updateAll();
	}

	/**
	 * Delete a premise from its selected graphic link.
	 */
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

	/**
	 * Delete the premise from its model object.
	 */
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

	/**
	 * Remove the currently selected view object irrespective of its nature.
	 */
	public function removeSelected(): Void {
		var s = graph.selectedElements();
		for (e in s) {
			if (e instanceof ArgumentBox) {
				removeArgumentFromBox((e as ArgumentBox).argument);
			}
			if (e instanceof StatementBox) {
				removeStatementFromBox((e as StatementBox).statement);
			}
			if (e instanceof ArgumentLink) {
				removeArgumentFromLink(e as ArgumentLink);
			}
			if (e instanceof PremiseLink) {
				deletePremiseFromLink(e as PremiseLink);
			}
		}
		//p unSelectAll();
		updateAll();
	}

	// Attribute Modification Functions
	// Hopefully self-explanatory

	// for statements
	/*p
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
	}*/

	public function changeStatementWff(s: Statement, c: String): Void {
		commands.do(
			ChangeStatementWffCommand {
				argumentGraph: argumentGraph
				statement: s
				wff: c
			}
		);
		updateView(
			GraphUpdate {
				changedAttribute: true
			}
		);
	};

	public function changeGraphTitle(g: ArgumentGraph, t: String): Void {
		commands.do(
			ChangeGraphTitleCommand {
				argumentGraph: g
				title: t
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

	public function changeStatementProofStandard(s: Statement, st: String): Void {
			if (st == proofStandardSE) {
				commands.do(
					ChangeStatementStandardCommand {
						argumentGraph: argumentGraph
						statement: s
						standard: Scintilla {}
					}
				);
			} else if (st == proofStandardDV) {
				commands.do(
					ChangeStatementStandardCommand {
						argumentGraph: argumentGraph
						statement: s
						standard: DialecticalValidity {}
					}
				);
			} else if (st == proofStandardBA) {
				commands.do(
					ChangeStatementStandardCommand {
						argumentGraph: argumentGraph
						statement: s
						standard: BestArgument {}
					}
				);
			} else if (st == proofStandardPE) {
				commands.do(
					ChangeStatementStandardCommand {
						argumentGraph: argumentGraph
						statement: s
						standard: Preponderance {}
					}
				);
			} else if (st == proofStandardCCE) {
				commands.do(
					ChangeStatementStandardCommand {
						argumentGraph: argumentGraph
						statement: s
						standard: ClearAndConvincingEvidence {}
					}
				);
			} else if (st == proofStandardBRD) {
				commands.do(
					ChangeStatementStandardCommand {
						argumentGraph: argumentGraph
						statement: s
						standard: BeyondReasonableDoubt {}
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
		//p updateView();
	}

	public function changeArgumentScheme(a: Argument, c: String): Void {
		commands.do(
			ChangeArgumentSchemeCommand {
				argumentGraph: argumentGraph
				argument: a
				scheme: c
			}
		);
		//p updateView();
	};

	public function changeArgumentTitle(a: Argument, t: String): Void {
		commands.do(
			ChangeArgumentTitleCommand {
				argument: a
				title: t
			}
		);
	}

	public function changeArgumentId(a: Argument, id: String): Void {
		var admissible: Boolean = true;

		if (not argumentGraph.noDoubleIDs(id)) {
			view.alert("The chosen id is already taken!");
			admissible = false;
		}

		if (id == "") {
			view.alert("id may not be empty.");
			admissible = false;
		}
		if (id.matches("^*[:alnum:][:space:]*[:alnum:]$")) {
			view.alert("id may not contain whitespaces.");
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

	 function graphIdTaken(id: String): Boolean {
		for (a in argumentGraphs) {
			if (a.id == id) { return true; }
		}
		return false;
	}

	 function getNewGraphId(): String {
		var admissible: Boolean = true;
		var id: String = "g";
		var number: Integer = 1;
		while ( graphIdTaken("{id}{number.toString()}") ) { number ++; }
		return "{id}{number.toString()}";
	}

	public function newGraph(): Void {
		graphs = [];
		addArgumentGraph(CarneadesControl.defaultGraph());
		graph = graphs[0];

		currentFile = null;

		commands.reset();
		unSelectAll();
		updateAll();
	}

	public function addArgumentGraph(newArgGraph: ArgumentGraph): Void {
		insert newArgGraph into argumentGraphs;
		var graph: CarneadesGraph = CarneadesGraph {
			visible: true
			control: bind this
			argumentGraph: newArgGraph
			glayout: TreeLayout {
				graph: bind graph
			}
		};
		graph.update();
		insert graph into graphs;
		updateView( GraphUpdate {
			listView: true
		} );
	}

	public function removeCurrentArgumentGraph(): Void {
		delete argumentGraph from argumentGraphs;
		delete graph from graphs;

		if (argumentGraphs != []) {
			displayGraph(argumentGraphs[0]);
		} else {
			newGraph();
		}

		updateAll();
	}

	/*p
	public function addArgumentGraph(newArgGraph: ArgumentGraph): Void {
		var graph: CarneadesGraph = CarneadesGraph {
			visible: true
			control: bind this
			argumentGraph: newArgGraph
			layout: TreeLayout {
				graph: bind graph
			}
		};
		graph.translateX = view.middleX;
		graph.translateY = view.middleY - yDistance;
		graph.update();
		insert graph into graphs;
		graph.layout.compose();
	}
	*/

	public function open(): Void {
		if (fileChanged) {
			var choice = JOptionPane.showOptionDialog(
								  null, "All changes to the graph will be lost.\nSave it now?" , "Save Changes?",
								  JOptionPane.YES_NO_CANCEL_OPTION,
								  JOptionPane.QUESTION_MESSAGE, null,
								  ["Save", "Don't Save", "Cancel"], null
							  );
			if (choice == JOptionPane.YES_OPTION) {
				saveAs();
			} else if (choice == JOptionPane.NO_OPTION) {
				var returnval = fileChooser.showOpenDialog(null);
				if (returnval == JFileChooser.APPROVE_OPTION) {
					loadGraphFromFile(fileChooser.getSelectedFile());
				}
			}
		} else {
			var returnval = fileChooser.showOpenDialog(null);
			if (returnval == JFileChooser.APPROVE_OPTION) {
				loadGraphFromFile(fileChooser.getSelectedFile());
			}
		}
	}
	
	public function loadGraphFromFile(f: File): Void {
		graphs = [];

		// set the current file
		currentFile = f;

		// load the graph
		var newArgGraphs: ArgumentGraph[] = ArgumentFile.getGraphFromFile(f);

		for (g in newArgGraphs) addArgumentGraph(g);

		graph = graphs[0];
		commands.reset();
		updateAll();
	}

	public function saveAs(): Void {
		var returnval = fileChooser.showSaveDialog(null);
		if (returnval == JFileChooser.APPROVE_OPTION) {
			var file: File = fileChooser.getSelectedFile();
				if (file.exists()) {
					var overwrite = JOptionPane.showOptionDialog(
						null, "The file already exists.\nDo you want to overwrite it?" , "Overwrite existing file?",
						JOptionPane.YES_NO_OPTION,
						JOptionPane.QUESTION_MESSAGE, null,
						["Yes", "No"], null
					);
				if (overwrite == JOptionPane.OK_OPTION) { saveAsGraphToFile(file); }
			} else { saveAsGraphToFile(file); }
		}
	}
	
	public function save(): Void {
		if (currentFile != null) {
			saveGraphToFile(currentFile);
		} else {
			saveAs();
		}
	}

	public function saveGraphToFile(f: File): Void {
		ArgumentFile.saveGraphToFile(argumentGraphs, f);
		currentFile = f;
		//p frame.title = "Carneades - { f.getAbsolutePath() }";
		fileChanged = false;
	}

	public function saveAsGraphToFile(f: File): Void {
		ArgumentFile.saveGraphToFile(argumentGraphs, f);
		currentFile = f;
		//p frame.title = "Carneades - { f.getAbsolutePath() }";
		fileChanged = false;
	}

	// DEBUG FUNCTIONS
	// This is misplaced here and should actually go into the model.
	public function defaultGraph(): ArgumentGraph {
		var argumentGraph = ArgumentGraph { id: getNewGraphId() };

		var s1: Statement = Statement {
			id: "s1"
			wff: "The street is wet."
		}

		var s2: Statement = Statement {
			id: "s2"
			wff: "It rained"
		}

		var a1: Argument = Argument {
			id: "a1"
			conclusion: s1
			title: "When it rains, things get wet."
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

	public function quit(): Void {
		if (fileChanged) {
			var choice = JOptionPane.showOptionDialog(
								  null, "All changes to the graph will be lost.\nSave it now?" , "Save Changes?",
								  JOptionPane.YES_NO_CANCEL_OPTION,
								  JOptionPane.QUESTION_MESSAGE, null,
								  ["Save", "Don't Save", "Cancel"], null
							  );
			if (choice == JOptionPane.YES_OPTION) {
				saveAs();
			} else if (choice == JOptionPane.NO_OPTION) {
				view.quit();
			}
		} else {
			view.quit();
		}
	}

	/*
	// DEBUG PRINT FUNCTIONS

	public function printSelected(): Void {
		System.out.println("vertices: {graph.selected}");
		System.out.println("models: {graph.selectedModels}");
		System.out.println("list: {frame.list.list.selectedItem}");
	}

	public function printSizes(): Void {
		System.out.println("view: {view.width}");
		System.out.println("layout: {graph.layout.width}");
	}
	*/

}

