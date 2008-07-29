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
import javafx.scene.*;
import javafx.scene.paint.*;
import java.lang.System;
import java.io.File;

// Model Classes
import Carneades.Argument.Argument;
import Carneades.Argument.Argument.*;

// Other View Classes
import Carneades.Graph.*;
import Carneades.Graph.Elements.Elements.*;

// Abstract Controller Class for Interaction
import Carneades.Control.GraphControl;


public class GraphList extends FlowPanel {

	//override attribute background = GC.panelBackground;
	override attribute alignment = HorizontalAlignment.LEFT;
	private attribute addButtonText: String = "";
	private attribute deleteButtonText: String = "";

	attribute control: GraphControl;
	attribute argumentGraph: ArgumentGraph;

	attribute input: SwingTextField = SwingTextField {
		preferredSize: bind [GC.editWidth-65, GC.textFieldHeight]
		visible: true
	}

	public attribute list: StatementList = StatementList {
		control: bind control
		preferredSize: bind [this.width-15, this.height - 70]
		visible: true
		statements: bind argumentGraph.statements
		filter: bind input.text.toLowerCase()
	}

	attribute addButton: SwingButton = SwingButton {
		text: bind addButtonText
		enabled: false
		preferredSize: [140, 20]
	}

	attribute deleteButton: SwingButton = SwingButton {
		text: bind deleteButtonText
		enabled: false
		preferredSize: [140, 20]
	}

	override attribute content = bind [ Label {text: "search "}, input, 
										list,
										addButton, deleteButton];

	public function getSelectedStatement(): Statement {
		return list.getSelectedStatement();
	}

	public function update() {
		//is a model selected
		if (sizeof control.getSelectedModel() > 0) {
			var model = control.getSelectedModel()[0];
			if (model instanceof Statement) {
				// add button
				addButtonText = "add argument";
				addButton.enabled = true;
				addButton.action = function(): Void {
					control.addArgumentToSelected();
				}
				// delete button
				deleteButtonText = { if (control.possibleToDelete) "delete" else "remove" };
				deleteButton.enabled = true;
				deleteButton.action = { 
					if (control.possibleToDelete) 
						function(): Void { control.deleteStatementFromList(); }
					else
						function(): Void { control.removeSelected(); }
				};
			} else if (model instanceof Argument) {
				// add button
				addButtonText = "add premise";
				addButton.enabled = true;
				addButton.action = function(): Void {
					control.addPremiseToSelected();
				}
				// delete button
				deleteButtonText = "delete";
				deleteButton.enabled = true;
				deleteButton.action = { 
					function(): Void { control.removeArgument(model as Argument); }
				};

			} else /*premise*/ {
				// add button
				// - nothing
				// delete button
				deleteButtonText = "delete";
				deleteButton.enabled = true;
				deleteButton.action = { function(): Void { control.deletePremise(model as Premise); } };
			}
		}
	}

	public function reset() {
		addButtonText = "";
		addButton.enabled = false;
		deleteButtonText = "";
		deleteButton.enabled = false;
	}
}

public class StatementList extends List {
	public attribute control: GraphControl;
	attribute filter: String = "";

	override attribute selectedItem = null on replace {
		if (selectedItem != null) {
			control.unSelectGraph();
			control.processSelection();
		}
	};

	attribute statements: Statement[];

	override attribute items = bind [ 
									for (s in statements where (matches(filter, s.id.toLowerCase()) or matches(filter, s.wff.toLowerCase()))) {
									 	StatementItem {
											statement: s
											text: bind "{s.wff}"
										}
									} 
								];

	public function getSelectedStatement(): Statement {
		if (selectedItem != null) {
			return (selectedItem as StatementItem).statement;
		} else {
			return null;
		}
	}

	private function matches(filter: String, text: String): Boolean {
		var result: Boolean = false;
		for (i in [0 .. (text.length()-1)]) {
			if (text.substring(i).startsWith(filter)) {
				result = true;
			}
		}
		return result;
	}
}

public class StatementItem extends ListItem {
	public attribute statement: Statement;
	attribute visible: Boolean = true;
}
