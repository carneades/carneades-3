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
import Carneades.Graph.GC.*;
import Carneades.Graph.Elements.Elements.*;

// Abstract Controller Class for Interaction
import Carneades.Control.GraphControl;

/**
 * The panel containing the list component in the upper right above the inspector panel listing the statements. It includes the two context-sensitive buttons below the list as well.
 */
public class ElementList extends FlowPanel {

	override attribute alignment = HorizontalAlignment.LEFT;

	// text displayed by the (left) addButton
	private attribute addButtonText: String = "";

	// text displayed by the (right) deleteButton
	private attribute deleteButtonText: String = "";

	attribute control: GraphControl;
	attribute argumentGraph: ArgumentGraph;

	/**
	 * The search string input field.
	 */
	private attribute input: SwingTextField = SwingTextField {
		preferredSize: bind [editWidth-65, textFieldHeight]
		visible: true
	}

	/**
	 * The embedded custom list component.
	 */
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

	/**
	 * Returns the selected statement from the list.
	 */
	public function getSelectedStatement(): Statement {
		return list.getSelectedStatement();
	}

	/**
	 * Refreshes the list display. Should be called after every alteration of the statements vector or a switching between graphs.
	 */
	public function update() {
		// is a model selected?
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
				deleteButtonText = "delete";
				deleteButton.enabled = true;
				deleteButton.action = { function(): Void { control.removeSelected(); } };
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

	/**
	 * Resets the buttons to blank and diables them.
	 */
	public function reset() {
		addButtonText = "";
		addButton.enabled = false;
		deleteButtonText = "";
		deleteButton.enabled = false;
	}
}

/**
 * The actual list component for the statement list to be embedded in the ElementList panel.
 */
public class StatementList extends List {
	
	/**
	 * The associated control component.
	 */
	public attribute control: GraphControl;
	
	/**
	 * The display filter string for the statement list.
	 */
	public attribute filter: String = "";

	override attribute selectedItem = null on replace {
		if (selectedItem != null) {
			control.unSelectGraph();
			control.processSelection();
		}
	};

	/**
	 * The sequence of statements to be displayed in the list.
	 */
	public attribute statements: Statement[];

	override attribute items = bind [ 
									for (s in statements 
											 where (matches(filter, s.id.toLowerCase()) 
													or matches(filter, s.wff.toLowerCase()))) {
									 	StatementItem {
											statement: s
											text: bind "{s.wff}"
										}
									} 
								];

	/**
	 * Returns the selected statement in the list.
	 */
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

/**
 * The list item class for the statement list.
 */
public class StatementItem extends ListItem {

	/**
	 * The model statement represented by the list item.
	 */
	public attribute statement: Statement;
	attribute visible: Boolean = true;
}
