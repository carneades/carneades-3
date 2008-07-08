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


package GraphSketch1.Graph;

import javafx.ext.swing.*;
import javafx.scene.paint.*;
import javafx.scene.*;

import java.lang.System;
import java.io.File;

// Model Classes
import GraphSketch1.Argument.Argument;
import GraphSketch1.Argument.Argument.*;

// Other View Classes
import GraphSketch1.Graph.*;
import GraphSketch1.Graph.Elements.Elements.*;

// Abstract Controller Class for Interaction
import GraphSketch1.Control.AbstractGraphControl;


public class GraphEdit extends Panel {

	override attribute x = 0;
	override attribute y = 0;
	override attribute visible = true;

	public attribute control: AbstractGraphControl;
	public attribute argumentGraph: ArgumentGraph;

	private attribute statementPanel = StatementEditPanel { control: bind control, argumentGraph: bind argumentGraph }
	private attribute argumentPanel = ArgumentEditPanel { control: bind control, argumentGraph: bind argumentGraph }
	private attribute premisePanel = PremiseEditPanel { control: bind control, argumentGraph: bind argumentGraph }

	override attribute content = bind [statementPanel, argumentPanel, premisePanel];

	public function update(): Void {
		if (control.getSelectedModel() != []) {
			if (control.getSelectedModel()[0] instanceof Argument) {
				statementPanel.visible = false;
				argumentPanel.visible = true;
				premisePanel.visible = false;
	
				var selected = control.getSelectedModel() [0];
				argumentPanel.loadArgument(selected as Argument);
			}
			else if (control.getSelectedModel()[0] instanceof Statement) {
	
				statementPanel.visible = true;
				argumentPanel.visible = false;
				premisePanel.visible = false;
	
				statementPanel.loadStatement(control.getSelectedModel() [0] as Statement);
			}
			else if (control.getSelectedModel()[0] instanceof Premise) {
				statementPanel.visible = false;
				argumentPanel.visible = false;
				premisePanel.visible = true;
	
				premisePanel.loadPremise(control.getSelectedModel() [0] as Premise);
			} else {
			}
		}
	}

	public function reset(): Void {/*
		toDo: If this is enabled, a double click on any ArgumentElement hides the edit menu. Fix this.

		statementPanel.visible = false;
		argumentPanel.visible = false;
		premisePanel.visible = false;
		*/
	}
}

public class EditPanel extends FlowPanel {
	override attribute alignment = HorizontalAlignment.LEFT;
	override attribute width = GC.editWidth;
	override attribute height = GC.editHeight;
	override attribute visible = false;
	protected attribute control: AbstractGraphControl;
	public attribute argumentGraph: ArgumentGraph;
}

public class StatementEditPanel extends EditPanel {
	attribute statement: Statement;
	private attribute editLabelWidth = bind GC.editLabelWidth;
	
	// General Components

	public attribute idField: IdField = IdField {
		preferredSize: [ GC.editWidth - GC.editLabelWidth - 30, 20 ]
		action: function(): Void {
			control.changeStatementId(statement, idField.text);
		}
	}
	
	public attribute contentField: ContentField = ContentField {
		preferredSize: [ GC.editWidth - GC.editLabelWidth - 30, 20 ]
		action: function(): Void {
			control.changeStatementWff(statement, contentField.text);
		}
	}


	private attribute acceptableBox: CheckBox = CheckBox {
		enabled: false
		preferredSize: [ GC.editWidth - GC.editLabelWidth - 30, 20 ]
		selected: bind statement.ok
	}

	// Proof Standard Components

	public attribute negatedBox: CheckBox = CheckBox {
		text: "negated"
		preferredSize: [ GC.editWidth - GC.editLabelWidth - 30, 20 ]
		action: function(): Void {
			submitStandard();
		}
	}

	public attribute complementBox: CheckBox = CheckBox {
		text: "complement"
		preferredSize: [ GC.editWidth - GC.editLabelWidth - 30, 20 ]
		action: function(): Void {
			submitStandard();
		}
	}

	public attribute proofStandardBox: ProofStandardField = ProofStandardField {
		preferredSize: [ GC.editWidth - GC.editLabelWidth - 30, 20 ]
		action: function(): Void {
			submitStandard();
		}
	}

	// temporary function to submit a new Proof Standard
	private function submitStandard(): Void {
		control.changeStatementProofStandard( statement,
											selectedStandard,
											{ if (negatedBox.selected) true else false },
											{ if (complementBox.selected) true else false });
	}

	private attribute standardGroup: ToggleGroup = ToggleGroup {};

	private attribute selectedStandard: String = bind (if (BAButton.selected) "BA" else if (SEButton.selected) "SE" else "DV");

	private attribute BAButton: RadioButton = RadioButton {
		toggleGroup: standardGroup
		text: "best argument"
		preferredSize: [ GC.editWidth - GC.editLabelWidth - 30, 20 ]
		action: function(): Void {
			submitStandard();
		}
	}

	private attribute SEButton: RadioButton = RadioButton {
		toggleGroup: standardGroup
		text: "scintilla of evidence"
		preferredSize: [ GC.editWidth - GC.editLabelWidth - 30, 20 ]
		action: function(): Void {
			submitStandard();
		}
	}

	private attribute DVButton: RadioButton = RadioButton {
		toggleGroup: standardGroup
		text: "dialectical validity"
		preferredSize: [ GC.editWidth - GC.editLabelWidth - 30, 20 ]
		action: function(): Void {
			submitStandard();
		}
	}

	// Status Components
	
	private attribute statusGroup: ToggleGroup = ToggleGroup {};

	private attribute statedButton: RadioButton = RadioButton {
		text: "stated"
		toggleGroup: statusGroup
		preferredSize: [ GC.editWidth - GC.editLabelWidth - 30, 20 ]
		action: function() {
			control.changeStatementStatus(statement, statedButton.text);
		}
	}
	private attribute questionedButton: RadioButton = RadioButton {
		text: "questioned"
		toggleGroup: statusGroup
		preferredSize: [ GC.editWidth - GC.editLabelWidth - 30, 20 ]
		action: function() {
			control.changeStatementStatus(statement, questionedButton.text);
		}
	}
	private attribute assumedTrueButton: RadioButton = RadioButton {
		text: "assumed true"
		toggleGroup: statusGroup
		preferredSize: [ GC.editWidth - GC.editLabelWidth - 30, 20 ]
		action: function() {
			control.changeStatementStatus(statement, assumedTrueButton.text);
		}
	}
	private attribute assumedFalseButton: RadioButton = RadioButton {
		text: "assumed false"
		toggleGroup: statusGroup
		preferredSize: [ GC.editWidth - GC.editLabelWidth - 30, 20 ]
		action: function() {
			control.changeStatementStatus(statement, assumedFalseButton.text);
		}
	}
	private attribute acceptedButton: RadioButton = RadioButton {
		text: "accepted"
		toggleGroup: statusGroup
		preferredSize: [ GC.editWidth - GC.editLabelWidth - 30, 20 ]
		action: function() {
			control.changeStatementStatus(statement, acceptedButton.text);
		}
	}
	private attribute rejectedButton: RadioButton = RadioButton {
		text: "rejected"
		toggleGroup: statusGroup
		preferredSize: [ GC.editWidth - GC.editLabelWidth - 30, 20 ]
		action: function() {
			control.changeStatementStatus(statement, rejectedButton.text);
		}
	}



	override attribute content = bind [ 
										Label { text: "id ", preferredSize: [editLabelWidth, 20] }, idField, 
										Label { text: "wff ", preferredSize: [editLabelWidth, 20] }, contentField,
										Label { text: "status ", preferredSize: [GC.editLabelWidth, 20] }, statedButton, 
										Label { text: "", preferredSize: [editLabelWidth, 20] }, questionedButton, 
										Label { text: "", preferredSize: [editLabelWidth, 20] }, assumedTrueButton, 
										Label { text: "", preferredSize: [editLabelWidth, 20] }, assumedFalseButton, 
										Label { text: "", preferredSize: [editLabelWidth, 20] }, acceptedButton, 
										Label { text: "", preferredSize: [editLabelWidth, 20] }, rejectedButton, 
										Label { text: "acceptable ", preferredSize: [editLabelWidth, 20] }, acceptableBox,
										Label { text: "proof standard", preferredSize: [editLabelWidth, 20] }, SEButton,
										Label { text: "", preferredSize: [editLabelWidth, 20] }, DVButton, 
										Label { text: "", preferredSize: [editLabelWidth, 20] }, BAButton, 
										Label { text: "", preferredSize: [editLabelWidth, 20] }, negatedBox,
										Label { text: "", preferredSize: [editLabelWidth, 20] }, complementBox,
										];

	// Functions

	public function loadStatement(s: Statement): Void {
		statement = s;
		idField.text = s.id;
		contentField.text = s.wff;

		if (statement.stated()) { statedButton.selected = true }
		if (statement.questioned()) { questionedButton.selected = true }
		if (statement.assumedTrue()) { assumedTrueButton.selected = true }
		if (statement.assumedFalse()) { assumedFalseButton.selected = true }
		if (statement.accepted()) { acceptedButton.selected = true }
		if (statement.rejected()) { rejectedButton.selected = true }
		
		if (statement.standard instanceof DialecticalValidity) { DVButton.selected = true; }
		else if (statement.standard instanceof Scintilla) { SEButton.selected = true; }
		else /*if (statement.standard instanceof Scintilla)*/ { BAButton.selected = true; }
		
		if (statement.standard.negated) { negatedBox.selected = true; } else { negatedBox.selected = false; }
		if (statement.standard.complement) { complementBox.selected = true; } else { complementBox.selected = false; }

	}
}

public class ArgumentEditPanel extends EditPanel {
	attribute argument: Argument;

	// Components

	public attribute idField: IdField = IdField {
		editable: true
		action: function(): Void {
			control.changeArgumentId(argument, idField.text);
		}
	}
	
	public attribute directionBox: DirectionField = DirectionField {
		action: function(): Void {
			if (directionBox.verified) { control.changeArgumentDirection(argument, directionBox.text); }
		}
	}

	public attribute defensibleLabel: Label = Label {
		text: bind { if (argument.ok) "true" else "false" }
		foreground: bind { if (argument.ok) Color.DARKGREEN else Color.DARKRED }
	}

	override attribute content = bind [ 
										Label { text: "id: " }, idField, 
										Label { text: "Direction: " }, directionBox,
										Label { text: "Defensible: " }, defensibleLabel
										];

	// Functions

	public function loadArgument(a: Argument): Void {
		argument = a;
		idField.text = argument.id;
		directionBox.text = { if (argument.pro) "pro" else "con" }
		defensibleLabel.text = { if (argument.allPremisesHold()) "true" else "false" };
	}
}

public class PremiseEditPanel extends EditPanel {
	attribute premise: Premise;

	// Components

	public attribute premiseBox: PremiseField = PremiseField {
		action: function(): Void {
			if (premiseBox.verified) { control.changePremiseType(premise, premiseBox.text); }
		}
	}
	
	public attribute roleField: RoleField = RoleField {
		editable: true
		action: function(): Void {
			control.changePremiseRole(premise, roleField.text);
		}
	}

	public attribute negationCheckBox: CheckBox = CheckBox {
		selected: premise.negative
		action: function(): Void {
			control.negatePremise(premise);
		}
	}

	override attribute content = bind [ 
										Label { text: "Role: " }, roleField, 
										Label { text: "Type: " }, premiseBox,
										Label { text: "Negated: " }, negationCheckBox,
										];

	// Functions

	public function loadPremise(p: Premise): Void {
		premise = p;
		premiseBox.text = { if (p.exception) "exception" else "ordinary" };
		roleField.text = p.role;
		negationCheckBox.selected = p.negative;
	}

}

// INTERMEDIATE COMPONENT CLASSES

class IdField extends TextField {
	override attribute visible = true;
	override attribute preferredSize = [GC.editWidth - 50, 20];
}

class ContentField extends TextField {
	override attribute editable = true;
	override attribute visible = true;
	override attribute preferredSize = [GC.editWidth - 80, 20];
}

class RoleField extends TextField {
	override attribute editable = false;
	override attribute visible = true;
	override attribute preferredSize = [GC.editWidth - 40, 20];
}

class BooleanBox extends ComboBox {
	public attribute value: Boolean;
	override attribute visible = true;
	override attribute items = [
		ComboBoxItem {
			text: "true"
			value: true
			selected: (value)
		},
		ComboBoxItem {
			text: "false"
			value: false
			selected: (value == false)
		}
	]
}

class DirectionBox extends ComboBox {
	public attribute pro: Boolean;
	override attribute visible = true;
	override attribute items = [
		ComboBoxItem {
			text: "Pro"
			value: true
			selected: (pro)
		},
		ComboBoxItem {
			text: "Con"
			value: false
			selected: (pro == false)
		}
	]
}

class ProofStandardBox extends ComboBox {
	public attribute standard: ProofStandard;
	override attribute visible = true;
	override attribute items = [
		ComboBoxItem {
			text: "DV"
			value: "DV"
			selected: bind (standard instanceof DialecticalValidity)
		},
		ComboBoxItem {
			text: "SE"
			value: "SE"
			selected: bind (standard instanceof Scintilla)
		},
		ComboBoxItem {
			text: "BA"
			value: "BA"
			selected: bind (standard instanceof BestArgument)
		}
	]
}

class StatusBox extends ComboBox {
	public attribute statement: Statement;
	override attribute visible = true;
	override attribute items = [
		ComboBoxItem {
			text: "stated"
			value: "stated"
			selected: bind (statement.value == "stated")
		},
		ComboBoxItem {
			text: "questioned"
			value: "questioned"
			selected: bind (statement.value == "questioned")
		},
		ComboBoxItem {
			text: "accepted"
			value: "accepted"
			selected: bind (statement.value == "accepted")
		},
		ComboBoxItem {
			text: "rejected"
			value: "rejected"
			selected: bind (statement.value == "rejected")
		},
	]
}

class PremiseBox extends ComboBox {
	public attribute premiseType: String = "";
	override attribute visible = true;
	override attribute items = [
		ComboBoxItem {
			text: "ordinary"
			value: "ordinary"
			selected: bind (premiseType == "ordinary")
		},
		ComboBoxItem {
			text: "exception"
			value: "exception"
			selected: bind (premiseType == "exception")
		},
	]
}

// Temporary Textfield components until the combobox gets an action attribute

class LimitedTextField extends TextField {
	attribute choices: String[] = [];
	override attribute foreground = bind { if (sizeof choices[c | c == this.text] == 1) Color.DARKGREEN else Color.DARKRED};
	attribute verified: Boolean = bind { (sizeof choices[c | c == this.text] == 1) };
}

class BooleanField extends LimitedTextField {
	override attribute preferredSize = [70, 20];
	override attribute choices = [ "true", "false" ];
}

class ProofStandardField extends LimitedTextField {
	override attribute preferredSize = [80, 20];
	override attribute choices = [ "SE", "DV", "BA" ];
}

class PremiseField extends LimitedTextField {
	override attribute preferredSize = [100, 20];
	override attribute choices = [ "ordinary", "exception" ];
}

class DirectionField extends LimitedTextField {
	override attribute preferredSize = [100, 20];
	override attribute choices = [ "pro", "con" ];
}

