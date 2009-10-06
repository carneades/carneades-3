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
import carneadesgui.control.ArgumentFile.*;

// File Chooser for Load/Save
import javax.swing.JFileChooser;
import javax.swing.JOptionPane;
import carneadesgui.CarneadesGUI;

import java.awt.Frame;
import javax.swing.JFrame;
import java.applet.Applet;
import java.awt.Container;
import javafx.geometry.Bounds;
import java.awt.image.BufferedImage;
import javax.imageio.ImageIO;

/**
 * Central control class for the Carneades application. It instantiates the needed view and model objects
 */

public class CarneadesControl {

    public var application: CarneadesGUI;

    // The application's model component. Should be set using setModel() in the application object post-init.
    public-read var model: CarneadesModel;
    public function setModel(model: CarneadesModel) { this.model = model; }

    // The application's views. Should be set using setView() from the CarneadesGUI object post-init.
    public-read var views: CarneadesView[] = [];
    public-read var view: CarneadesView;
    public function addView(view: CarneadesView): Void {
		view.control = this;
		insert view into views;
    }
    public function setActiveView(view: CarneadesView): Void {
		for (v in views) v.deactivate();
		view.activate();
		this.view = view;
    }
    public function alternateView(): Void {

		var oldView: CarneadesView = view;
		var newView: CarneadesView;

		// determine old view
		var index: Integer = 0;
		for (i in [0 .. sizeof views - 1]) if (views[i] == view) index = i;

		// deactivate old view
		oldView.deactivate();

		// determine new view
		if (index == sizeof views -1) index = 0 else index++;
		newView = views[index];
		newView.activate();

		// take over the array of viewgraphs
		var cg: CarneadesGraph = oldView.currentGraph;
		oldView.currentGraph = null;
		var gs: CarneadesGraph[] = oldView.graphs;
		oldView.graphs = null;

		newView.graphs = gs;
		newView.currentGraph = cg;

		// set
		this.view = newView;
		updateAll();
    }

    // The array of currently loaded model argument graphs in the model component. Read-Only.
    public def argumentGraphs: ArgumentGraph[] = bind model.argumentGraphs;

    // The current model argument graph that is to be displayed. Read-Only.
    public def argumentGraph: ArgumentGraph = bind graph.argumentGraph;

    // The array of graph objects that correspond to the model graphs.
    public def graphs: CarneadesGraph[] = bind view.graphs;

    // The view graph object currently displayed.
    public def graph: CarneadesGraph = bind view.currentGraph;

    // The command administering unit of the currently displayed graph.
    def commands: CommandControl = bind graph.commands;

    // View configuration attributes
    public var possibleToRemove: Boolean = false ;
    public var possibleToUndo: Boolean = false;
    public var possibleToRedo: Boolean = false;
	public def possibleToAddArgument = bind graph.selectedModel instanceof Statement;
    public def possibleToAddPremise = bind graph.selectedModel instanceof Argument;

    // navigation mg: currently idle
    public var dragView: Boolean = false;

    // SELECTION
    function singleStatementSelected(): Boolean { graph.selectedModel instanceof Statement }
    function singleArgumentSelected(): Boolean { graph.selectedModel instanceof Argument }
	function singlePremiseSelected(): Boolean {
			not (sizeof graph.selectedElements() != 1) and (graph.selectedElements()[0] instanceof PremiseLink)}
    function premiseSelected(): Boolean {
			not (sizeof graph.selectedElements() != 1) and (graph.selectedElements()[0] instanceof PremiseLink)}
    function singleSomethingSelected(): Boolean { (sizeof graph.selectedElements() == 1) }
    function nothingSelected(): Boolean { (sizeof graph.selectedElements() == 0); }

    public function processGraphSelection(g: GraphElement): Void {
		unSelectAll();
		g.selected = true;
		graph.updateSelectedModelsFromElements();
		if (not view.isVisible(g)) focusOnSelected();
		updateView( GraphUpdate { graphSelection: true } );
    }

    public function processListSelection(e: Object): Void {
		unSelectGraph();
		if (not (e instanceof ArgumentGraph)) {
			graph.selectedModel = e;
			graph.updateSelectedElementFromModel();
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
    public function unSelectGraph(): Void { graph.unSelectAll(); }

    /**
     * Focus the view on a certain view object.
     */
    public function focusOnElement(e: GraphElement) { view.focusOn(e); }

    /**
     * Focus the view on the selected view object.
     */
    public function focusOnSelected() { view.focusOn(graph.selectedElements()[0]); }

    // UPDATE FUNCTIONS

    function updateView(u: GraphUpdate): Void {
		if (u.layout) {
			// 1. Rendering update
			graph.updateFromModel();
			graph.updateDisplay();

			// 2. Restore Selection
			graph.updateSelectedElementFromModel();
		}

		// set modes
		if (singleStatementSelected()) view.editStatement(graph.selectedModel as Statement);
		if (singleArgumentSelected()) view.editArgument(graph.selectedModel as Argument);
		if (singlePremiseSelected()) view.editPremise(graph.selectedModel as Premise);
		if (nothingSelected()) view.editNothing();

		// update the view component influencing booleans
		possibleToUndo = commands.possibleToUndo();
		possibleToRedo = commands.possibleToRedo();
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

    // Perform an undo action.
    public function undo(): Number {
		var result = commands.undo();
		updateAll();
		return result;
    }

    // Perform a redo action.
    public function redo(): Number {
		var result = commands.redo();
		updateAll();
		return result;
    }


    // Switch the view to another argument graph.
    public function displayGraph(a: ArgumentGraph) {
		view.currentGraph = (for (g in graphs where (g as CarneadesGraph).argumentGraph == a) { g }) [0];
		updateAll();
    }

	public function displayGraphListView() {
		view.displayGraphListView()
	}


    // MODEL MANIPULATION FUNCTIONS

    // DRAGGING

    // drag & drop
    public var draggingOver: Object = false;
    public var dragging: Boolean = false;
    public var canDrop: Boolean = false;

    public function setDraggingOver(thing): Void {
		draggingOver = thing;
		canDrop = canDropOn(thing);
    }

    public function canDropOn(target): Boolean {
		// there is something to drop on
		target != null

		// something is not dropped on itself
		and graph.selectedElements()[0] != target

		// they are not of different types
		and {
			((graph.selectedElements()[0] instanceof StatementBox) and not (target instanceof StatementBox))
			or ((graph.selectedElements()[0] instanceof ArgumentBox) and not (target instanceof ArgumentBox))
		}
	}


		// Processes the start of a dragging action. Should be called whenever a dragging action starts.
	public function startDrag(thing): Void {
		dragging = true;
	}

		// Processes the end of a dragging action. Should be called when a dragging action ends.
	public function endDrag(shiftDown: Boolean): Void {
		if (dragging) {
			dragging = false;
			// Were we dragging over something?
			if (draggingOver != null) {
				dragEndsAt(draggingOver as ArgumentElement, shiftDown);
				draggingOver = null;
			}
		}
		canDrop = false;
    }

    function dragEndsAt(target, shiftDown: Boolean): Void {
		var selected = graph.selectedElements();
		if (target != selected) {
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
						if (shiftDown) { commands.do(
							CopyPremiseCommand {
								argumentGraph: argumentGraph
								premise: premise
								newArgument: newArgument
							})
						} else commands.do(
							MovePremiseCommand {
								argumentGraph: argumentGraph
								premise: premise
								oldArgument: oldArgument
								newArgument: newArgument
							});

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
							});

						// 2. check for cycles and undo in case
						if (not argumentGraph.noCycles()) {
							commands.undo();
							commands.pop();
							view.alert("No! The Graph would become cyclic.");
						}
					}
				}
			}
		}
		unSelectAll();
		updateAll();
    }

    // CORE MODEL MANIPULATION

    /**
     * Adds a blank statement to the graph.
     */
    public function addStatement(): Void {
		commands.do(
			AddStatementCommand {
				argumentGraph: argumentGraph
			});
		updateAll();
    }

    /**
     * Append an argument to the selected statement.
     */
    public function addArgumentToSelected(): Void {
		if (graph.selectedModel instanceof Statement) {
			if (commands.do(
			AddArgumentAndPremiseCommand {
				argumentGraph: argumentGraph
				statement: graph.selectedModel as Statement
			}) != C_OK) {
			view.alert("Argument cannot be inserted here.\nThe Graph would become cyclic.");
			} else updateAll();
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
			});
		}
		updateAll();
    }

    // DELETION FUNCTIONS
    /**
     * Removes an argument from a marked view argument node.
     */
    public function removeArgumentFromBox(ar: Argument): Void {
		var a: Argument = null;
		if (ar != null) a = ar else a = graph.selectedModel as Argument;
		commands.do(
			RemoveArgumentCommand {
				argumentGraph: argumentGraph
				argument: a
			});
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
			});
		unSelectAll();
		updateAll();
    }

    /**
     * Remove a statement from whose view object has been selected.
     */
    public function removeStatementFromBox(sb: StatementBox): Void {

		if (argumentGraph.isConclusion(sb.statement)) {
			// If the statement is the conclusion of an argument,
			// delete the premise as well as the arguments leading to it.
			commands.do(
			DeleteConclusionCommand {
				argumentGraph: argumentGraph
				conclusion: sb.statement
				motherArgument: {if (sb.parentVertex != graph.root) (sb.parentVertex as ArgumentBox).argument else null}
				childArguments: argumentGraph.arguments[a | a.conclusion == sb.statement ]
			});
		} else if (argumentGraph.isPremise(sb.statement)){

			// get corresponding model premise and argument
			var argument: Argument = (sb.parentVertex as ArgumentBox).argument;
			//var premise: Premise = [for (p in argument.premises where p.statement == sb.statement) p as Premise][0];
			var premise: Premise = argument.premises[p | p.statement == sb.statement][0];
			
			// if there is one instance left, arrange duplicate ramifications
			commands.do(
			DeletePremiseStatementCommand {
				argumentGraph: argumentGraph
				argument: argument
				premise: premise
			});
			
		} else {
			// Otherwise, delete the statement only.
			commands.do(
			DeleteStatementCommand {
				argumentGraph: argumentGraph
				statement: sb.statement
			});
		}

		unSelectAll();
		updateAll();
    }

    /**
     * Remove an argument from its selected link.
     */
    public function removeArgumentFromLink(l: ArgumentLink): Void {
		commands.do(
			RemoveArgumentCommand {
				argumentGraph: argumentGraph
				argument: (l.producer as ArgumentBox).argument
			});
		unSelectAll();
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
					});
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
					});
				}
			}
		}
    }

    /**
     * Remove the currently selected view object irrespective of its nature.
     */
    public function removeSelected(): Void {
		var e: GraphElement = graph.selectedElements()[0];
		
		if (e instanceof ArgumentBox) {
			removeArgumentFromBox((e as ArgumentBox).argument);
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

    // Attribute Modification Functions
    // Hopefully self-explanatory

    // for statements
    public function changeStatementId(s: Statement, id: String): Void {
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
			ChangeStatementIdCommand {
				argumentGraph: argumentGraph
				statement: s
				id: id
			});
		}
		updateAll();
    }

    public function changeStatementWff(s: Statement, c: String): Void {
		commands.do(
			ChangeStatementWffCommand {
				argumentGraph: argumentGraph
				statement: s
				wff: c
			});
		updateView(
			GraphUpdate {
				changedAttribute: true
			});
    }

    public function changeGraphTitle(g: ArgumentGraph, t: String): Void {
		commands.do(
			ChangeGraphTitleCommand {
				argumentGraph: g
				title: t
			});
		updateAll();
    };

    public function changeStatementStatus(s: Statement, v: String): Void {
		commands.do(
			ChangeStatementStatusCommand {
				argumentGraph: argumentGraph
				statement: s
			newStatus: v
			});
		updateAll();
    }

    public function changeStatementProofStandard(s: Statement, st: String): Void {
		if (st == proofStandardSE) {
			commands.do(
			ChangeStatementStandardCommand {
				argumentGraph: argumentGraph
				statement: s
				standard: Scintilla {}
			});
		} else if (st == proofStandardDV) {
			commands.do(
			ChangeStatementStandardCommand {
				argumentGraph: argumentGraph
				statement: s
				standard: DialecticalValidity {}
			});
		} else if (st == proofStandardPE) {
			commands.do(
			ChangeStatementStandardCommand {
				argumentGraph: argumentGraph
				statement: s
				standard: Preponderance {}
			});
		} else if (st == proofStandardCCE) {
			commands.do(
			ChangeStatementStandardCommand {
				argumentGraph: argumentGraph
				statement: s
				standard: ClearAndConvincingEvidence {}
			});
		} else if (st == proofStandardBRD) {
			commands.do(
			ChangeStatementStandardCommand {
				argumentGraph: argumentGraph
				statement: s
				standard: BeyondReasonableDoubt {}
			});
		}
		updateAll();
    }

    // for arguments

    public function changeArgumentDirection(a: Argument, value: String): Void {
		var newValue: Boolean = { if (value == "pro") true else false };
		if (newValue != a.pro) {
			commands.do(
			ChangeArgumentDirectionCommand {
				argumentGraph: argumentGraph
				argument: a
			});
		}
		updateAll();
    }

    public function changeArgumentWeight(a: Argument, v: Number): Void {
		commands.do(
			ChangeArgumentWeightCommand {
				argumentGraph: argumentGraph
				argument: a
				weight: v
			});
    }

    public function changeArgumentScheme(a: Argument, c: String): Void {
		commands.do(
			ChangeArgumentSchemeCommand {
				argumentGraph: argumentGraph
				argument: a
				scheme: c
			});
    };

    public function changeArgumentTitle(a: Argument, t: String): Void {
		commands.do(
			ChangeArgumentTitleCommand {
				argument: a
				title: t
			});
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

    public function getNewGraphId(): String {
		var id: String = "g";
		var number: Integer = 1;
		while ( graphIdTaken("{id}{number.toString()}") ) { number ++; }
		return "{id}{number.toString()}";
    }

    public function newGraph(): Void {
		view.graphs = [];
		addArgumentGraph(defaultArgumentGraph(getNewGraphId()));
		currentFile = null;
		commands.reset();
    }

    public function addArgumentGraph(newArgGraph: ArgumentGraph): Void {
		insert newArgGraph into model.argumentGraphs;
		var graph: CarneadesGraph = CarneadesGraph {
			visible: true
			control: bind this
			argumentGraph: newArgGraph
			glayout: TreeLayout {
				graph: bind graph
			}
		};
		insert graph into view.graphs;
		view.currentGraph = graphs[0];
		unSelectAll();
		updateAll()
    }

    public function removeCurrentArgumentGraph(): Void {
		delete argumentGraph from model.argumentGraphs;
		delete graph from view.graphs;

		if (argumentGraphs != []) {
			displayGraph(argumentGraphs[0]);
		} else {
			newGraph();
		}
		updateAll();
    }

    // FILE LOAD, SAVE & QUIT

    public var currentFile: File = null;
    public var fileChanged: Boolean = false;
    public var fileLoaded: Boolean = bind currentFile != null;
    public var fileChooser: JFileChooser = new JFileChooser();

	public function newDocument(): Void {
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
                newGraph();
            }
        } else {
            newGraph();
        }
    }

    public function open(): Void {
		if (fileChanged) {
			var choice = JOptionPane.showOptionDialog(
				null, "All changes to the graph will be lost.\nSave it now?" , "Save Changes?",
				JOptionPane.YES_NO_CANCEL_OPTION,
				JOptionPane.QUESTION_MESSAGE, null,
				["Save", "Don't Save", "Cancel"], null);
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
		view.graphs = [];

		// set the current file
		currentFile = f;

		// load the graph
		var newArgGraphs: ArgumentGraph[] = ArgumentFile.getGraphFromFile(f);

		for (g in newArgGraphs) addArgumentGraph(g);

		view.currentGraph = view.graphs[0];
		commands.reset();
		view.displayTitle = f.getAbsolutePath();
		updateAll();
	}

	public function saveAs(): Void {
		var returnval = fileChooser.showSaveDialog(null);
		var file: File;
		if (returnval == JFileChooser.APPROVE_OPTION) {
			file = fileChooser.getSelectedFile();
			if (file.exists()) {
				var overwrite = JOptionPane.showOptionDialog(
					null, "The file already exists.\nDo you want to overwrite it?" , "Overwrite existing file?",
					JOptionPane.YES_NO_OPTION,
					JOptionPane.QUESTION_MESSAGE, null,
					["Yes", "No"], null
					);
				if (overwrite == JOptionPane.OK_OPTION) {
					saveAsGraphToFile(file);
					view.displayTitle = file.getAbsolutePath();
				}
			} else {
				saveAsGraphToFile(file);
				view.displayTitle = file.getAbsolutePath();
			}
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
		fileChanged = false;
    }

    public function saveAsGraphToFile(f: File): Void {
		ArgumentFile.saveGraphToFile(argumentGraphs, f);
		currentFile = f;
		fileChanged = false;
    }

    public function quit(): Void {
		if (fileChanged) {
			var choice = JOptionPane.showOptionDialog(
				null, "All changes to the graph will be lost.\nSave it now?" , "Save Changes?",
				JOptionPane.YES_NO_CANCEL_OPTION,
				JOptionPane.QUESTION_MESSAGE, null,
				["Save", "Don't Save", "Cancel"], null);
			if (choice == JOptionPane.YES_OPTION) {
				if (currentFile != null) {
					save();
					application.quit();
				} else saveAs();
			} else if (choice == JOptionPane.NO_OPTION) {
				application.quit();
			}
		} else {
			application.quit();
		}
    }

	public function saveGraphAsImage(): Void {
		saveAsImage(graph);
	}

	function getContainer() : Container {
		var container : Container;
		if("{__PROFILE__}" == "browser") { // Applet
			container = FX.getArgument("javafx.applet") as Applet;
		} else { // Standalone
			var frames = Frame.getFrames();
			// We may improve this logic so as to find the
			// exact Stage (Frame) based on its title
			container = (frames[0] as JFrame).getContentPane();
		}
		return container;
	}

	function save(container : Container, bounds : Bounds, file : File) {

		var bufferedImage = new BufferedImage(
			bounds.width, bounds.height, BufferedImage.TYPE_INT_ARGB);
		var graphics = bufferedImage.getGraphics();
		graphics.translate(-bounds.minX, -bounds.minY);
		container.paint(graphics);
		graphics.dispose();

		ImageIO.write(bufferedImage, "png", file);
	}

    public function defaultArgumentGraph(id: String): ArgumentGraph {
		var argumentGraph = ArgumentGraph {
			id: id
		}

		var a1: Argument;
		var a2: Argument;

		var s1: Statement = Statement {
			id: "s1"
			wff: "Conclusion"
		}

		var s2: Statement = Statement {
			id: "s2"
			wff: "Premise Statement"
			//arguments: [a1, a2]
		}

		a1 = Argument {
			id: "a1"
			conclusion: s1
			title: "... therefore ..."
		}

		var p1a1: Premise = Premise {
			statement: s2
		}

		a1.addPremise(p1a1);

		argumentGraph.insertStatement(s1);
		argumentGraph.insertStatement(s2);
		argumentGraph.insertArgument(a1);

		return argumentGraph;
    }
}

