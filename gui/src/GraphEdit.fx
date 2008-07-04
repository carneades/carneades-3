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

//import javafx.ext.swing.*;
//import javafx.scene.paint.*;
//import javafx.scene.*;
import javafx.gui.*;
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
	override attribute preferredSize = [GC.editWidth, GC.editHeight];
	override attribute visible = true;

	private attribute control: AbstractGraphControl;
	public attribute argumentGraph: ArgumentGraph;

	private attribute statementPanel = StatementEditPanel { control: bind control, argumentGraph: bind argumentGraph }
	private attribute argumentPanel = ArgumentEditPanel { control: bind control, argumentGraph: bind argumentGraph }
	private attribute premisePanel = PremiseEditPanel { control: bind control, argumentGraph: bind argumentGraph }

	override attribute content = bind [statementPanel, argumentPanel, premisePanel];

	public function update(): Void {
		if (control.getSelectedModel() <> []) {
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
	
	// Components

	public attribute idField: IdField = IdField {
		action: function(): Void {
			control.changeStatementId(statement, idField.text);
		}
	}
	
	public attribute contentField: ContentField = ContentField {
		action: function(): Void {
			control.changeStatementWff(statement, contentField.text);
		}
	}

	public attribute negatedBox: BooleanField = BooleanField {
		preferredSize: [100, 20]
		action: function(): Void {
			submitStandard();
		}
	}

	public attribute complementBox: BooleanField = BooleanField {
		preferredSize: [100, 20]
		action: function(): Void {
			submitStandard();
		}
	}

	public attribute assumptionBox: BooleanField = BooleanField {
		action: function(): Void {
			if (assumptionBox.verified) { 
				control.changeStatementAssumption(statement, assumptionBox.text); 
				// update the other two fields.
				statusBox.update();
				valueBox.text = statement.value;
			}
		}
	}

	public attribute valueBox: ValueField = ValueField {
		preferredSize: [100, 20]
		action: function(): Void {
			if (valueBox.verified) { 
				control.changeStatementValue(statement, valueBox.text); 
				// update the other two fields.
				statusBox.update();
				assumptionBox.text = {if (statement.assumption) "true" else "false"};
			}
		}
	}

	public attribute statusBox: StatusField = StatusField {
		preferredSize: [120, 20];
		action: function(): Void {
			if (statusBox.verified) { 
				control.changeStatementStatus(statement, statusBox.text); 
				// update the other two fields.
				valueBox.text = statement.value;
				assumptionBox.text = {if (statement.assumption) "true" else "false"};
			}
		}
	}

	public attribute acceptableLabel: Label = Label {
		text: bind {if (statement.ok) "true" else "false" }
		foreground: bind {if (statement.ok) Color.DARKGREEN else Color.DARKRED }
	}

	public attribute proofStandardBox: ProofStandardField = ProofStandardField {
		action: function(): Void {
			submitStandard();
		}
	}

	// temporary function to submit a new Proof Standard
	private function submitStandard(): Void {
		if (proofStandardBox.verified and complementBox.verified and negatedBox.verified) {
			control.changeStatementProofStandard(	statement,
											proofStandardBox.text,
											{ if (negatedBox.text == "true") true else false },
											{ if (complementBox.text == "true") true else false });
		}
	}

	override attribute content = bind [ 
										Label { text: "id: " }, idField, 
										Label { text: "wff: " }, contentField,
										Label { text: "Value: " }, valueBox,
										Label { text: "Assumption: " }, assumptionBox,
										Label { text: "Status: " }, statusBox,
										Label { text: "Acceptable: " }, acceptableLabel,
										Label { text: "Standard of Proof:       "},
										Label { text: "Standard: " }, proofStandardBox,
										Label { text: "Negated: " }, negatedBox,
										Label { text: "Complement: " }, complementBox,
										];

	// Functions

	public function loadStatement(s: Statement): Void {
		statement = s;
		idField.text = s.id;
		contentField.text = s.wff;
		//valueBox.value = s.value;
		valueBox.text = s.value;
		//assumptionBox.value = s.assumption;
		assumptionBox.text = {if (s.assumption) "true" else "false"};
		//statusBox.statement = s;
		statusBox.statement = s;
		statusBox.update();
		negatedBox.text = {if (s.standard.negated) "true" else "false"};
		complementBox.text = {if (s.standard.complement) "true" else "false"};
		proofStandardBox.text = { 	if (s.standard instanceof DialecticalValidity) "DV"
									else if (s.standard instanceof BestArgument) "BA"
									else /*(s.standard instanceof Scintilla)*/ "SE"};
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

class ValueBox extends ComboBox {
	public attribute value: String = "";
	override attribute visible = true;
	override attribute items = [
		ComboBoxItem {
			text: "true"
			value: "true"
			selected: bind (value == "true")
		},
		ComboBoxItem {
			text: "false"
			value: "false"
			selected: bind (value == "false")
		},
		ComboBoxItem {
			text: "unknown"
			value: "unknown"
			selected: bind (value == "unknown")
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

class ValueField extends LimitedTextField {
	override attribute choices = [ "true", "false", "unknown"];
}

class StatusField extends LimitedTextField {
	attribute statement: Statement = Statement {} on replace { update(); }
	override attribute choices = ["stated", "accepted", "rejected", "questioned"];

	public function update(): Void {
		text = statement.getStatus();
	}
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




