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
import javafx.scene.*;

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


public class GraphEdit extends Panel {

	override attribute x = 0;
	override attribute y = 0;
	override attribute visible = true;

	public attribute control: GraphControl;
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

	public function reset(): Void {
		statementPanel.visible = false;
		argumentPanel.visible = false;
		premisePanel.visible = false;
		
	}
}

public class EditPanel extends FlowPanel {
	//override attribute background = GC.panelBackground;
	override attribute alignment = HorizontalAlignment.LEFT;
	override attribute width = GC.editWidth;
	override attribute height = GC.editHeight;
	override attribute visible = false;
	protected attribute control: GraphControl;
	public attribute argumentGraph: ArgumentGraph;
	protected attribute editLabelWidth = bind GC.editLabelWidth;
	protected attribute editComponentWidth = bind GC.editWidth - editLabelWidth - 30;

}

public class StatementEditPanel extends EditPanel {
	attribute statement: Statement;
	
	// General Components

	private attribute idField: IdField = IdField {
		preferredSize: [ GC.editWidth - GC.editLabelWidth - 30, 20 ]
		action: function(): Void {
			control.changeStatementId(statement, idField.text);
		}
	}
	
	private attribute contentField: ContentField = ContentField {
		preferredSize: [ editComponentWidth, 20 ]
		action: function(): Void {
			control.changeStatementWff(statement, contentField.text);
		}
	}


	private attribute acceptableBox: CheckBox = CCheckBox {
		enabled: false
		preferredSize: [ GC.editWidth - GC.editLabelWidth - 30, 20 ]
		selected: bind statement.ok
	}

	// Proof Standard Components

	private attribute negatedBox: CheckBox = CCheckBox {
		text: "negated"
		preferredSize: [ editComponentWidth, 20 ]
		action: function(): Void {
			submitStandard();
		}
	}

	private attribute complementBox: CheckBox = CCheckBox {
		text: "complement"
		preferredSize: [ editComponentWidth - 30, 20 ]
		action: function(): Void {
			submitStandard();
		}
	}

	private attribute proofStandardBox: ProofStandardField = ProofStandardField {
		preferredSize: [ editComponentWidth - 30, 20 ]
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

	private attribute BAButton: RadioButton = CRadioButton {
		toggleGroup: standardGroup
		text: "best argument"
		preferredSize: [ editComponentWidth, 20 ]
		action: function(): Void {
			submitStandard();
		}
	}

	private attribute SEButton: RadioButton = CRadioButton {
		toggleGroup: standardGroup
		text: "scintilla of evidence"
		preferredSize: [ editComponentWidth, 20 ]
		action: function(): Void {
			submitStandard();
		}
	}

	private attribute DVButton: RadioButton = CRadioButton {
		toggleGroup: standardGroup
		text: "dialectical validity"
		preferredSize: [ editComponentWidth - 30, 20 ]
		action: function(): Void {
			submitStandard();
		}
	}

	// Status Components
	
	private attribute statusGroup: ToggleGroup = ToggleGroup {};

	private attribute statedButton: RadioButton = CRadioButton {
		text: "stated"
		toggleGroup: statusGroup
		preferredSize: [ editComponentWidth, 20 ]
		action: function() {
			control.changeStatementStatus(statement, statedButton.text);
		}
	}
	private attribute questionedButton: RadioButton = CRadioButton {
		text: "questioned"
		toggleGroup: statusGroup
		preferredSize: [ editComponentWidth, 20 ]
		action: function() {
			control.changeStatementStatus(statement, questionedButton.text);
		}
	}
	private attribute assumedTrueButton: RadioButton = CRadioButton {
		text: "assumed true"
		toggleGroup: statusGroup
		preferredSize: [ editComponentWidth, 20 ]
		action: function() {
			control.changeStatementStatus(statement, assumedTrueButton.text);
		}
	}
	private attribute assumedFalseButton: RadioButton = CRadioButton {
		text: "assumed false"
		toggleGroup: statusGroup
		preferredSize: [ editComponentWidth, 20 ]
		action: function() {
			control.changeStatementStatus(statement, assumedFalseButton.text);
		}
	}
	private attribute acceptedButton: RadioButton = CRadioButton {
		text: "accepted"
		toggleGroup: statusGroup
		preferredSize: [ editComponentWidth, 20 ]
		action: function() {
			control.changeStatementStatus(statement, acceptedButton.text);
		}
	}
	private attribute rejectedButton: RadioButton = CRadioButton {
		text: "rejected"
		toggleGroup: statusGroup
		preferredSize: [ editComponentWidth, 20 ]
		action: function() {
			control.changeStatementStatus(statement, rejectedButton.text);
		}
	}



	override attribute content = bind [ 
										Label { text: "id ", preferredSize: [editLabelWidth, 20] }, idField, 
										Label { text: "content ", preferredSize: [editLabelWidth, 20] }, contentField,
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
		preferredSize: [ editComponentWidth, 20 ]
		editable: true
		action: function(): Void {
			control.changeArgumentId(argument, idField.text);
		}
	}
	
	public attribute defensibleBox: CheckBox = CCheckBox {
		preferredSize: [ editComponentWidth, 20 ]
		enabled: false
		selected: bind argument.ok
	}

	private attribute directionGroup: ToggleGroup = ToggleGroup {};

	private attribute proButton: RadioButton = CRadioButton {
		toggleGroup: directionGroup
		text: "pro"
		action: function(): Void {
			control.changeArgumentDirection(argument, "pro");
		}
	}

	private attribute conButton: RadioButton = CRadioButton {
		toggleGroup: directionGroup
		text: "con"
		action: function(): Void {
			control.changeArgumentDirection(argument, "con");
		}

	}

	private attribute weightSlider: WeightSlider = WeightSlider {
		argument: bind argument
		control: bind control
		preferredSize: [ editComponentWidth-35, 20 ]
		maximum: 100
		minimum: 0
	}

	private attribute weightNumber: TextField = TextField {
		preferredSize: [ 30, 20 ]
		editable: false
		text: bind (weightSlider.value as Number).toString()
	}

	override attribute content = bind [ 
										Label { text: "id " preferredSize: [editLabelWidth, 20]}, idField, 
										Label { text: "direction ", preferredSize: [editLabelWidth, 20]}, proButton, conButton,
										Label { text: "defensible ", preferredSize: [editLabelWidth, 20]}, defensibleBox,
										Label { text: "weight ", preferredSize: [editLabelWidth, 20]}, weightSlider, weightNumber
										];

	// Functions

	public function loadArgument(a: Argument): Void {
		argument = a;
		idField.text = argument.id;

		if (a.pro) { 
			proButton.selected = true;
			conButton.selected = false;
		} else {
			proButton.selected = false;
			conButton.selected = true;
		}

		weightSlider.setValue(argument.weight as Integer);
	}
}

public class PremiseEditPanel extends EditPanel {
	attribute premise: Premise;

	// Components
	public attribute roleField: RoleField = RoleField {
		preferredSize: [ editComponentWidth, 20 ]
		editable: true
		action: function(): Void {
			control.changePremiseRole(premise, roleField.text);
		}
	}

	public attribute exceptionBox: CheckBox = CCheckBox {
		preferredSize: [ editComponentWidth, 20 ]
		selected: premise.exception
		action: function(): Void {
			control.changePremiseType(premise, exceptionBox.selected);
		}
	}

	public attribute negationBox: CheckBox = CCheckBox {
		preferredSize: [ editComponentWidth, 20 ]
		selected: premise.negative
		action: function(): Void {
			control.negatePremise(premise);
		}
	}

	override attribute content = bind [ 
										Label { text: "role ", preferredSize: [editLabelWidth, 20] }, roleField, 
										Label { text: "exception ", preferredSize: [editLabelWidth, 20] }, exceptionBox,
										Label { text: "negated ", preferredSize: [editLabelWidth, 20] }, negationBox,
										];

	// Functions

	public function loadPremise(p: Premise): Void {
		premise = p;
		roleField.text = p.role;
		negationBox.selected = p.negative;
		exceptionBox.selected = p.exception;
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

class WeightSlider extends Slider {
	public attribute argument: Argument = Argument {} on replace { value = (argument.weight) as Integer; }
	private attribute updateChange: Boolean = true; // needs to be true to avoid inital command dispatch from on replace value
	override attribute value = (argument.weight) as Integer on replace { submitWeight(); }
	public attribute control: GraphControl;

	function setValue(v: Integer) {
		updateChange = true;
		value = v;
	}

	function submitWeight(): Void {
		if (not updateChange) { 
			control.changeArgumentWeight(argument, value); 
		} else {
			updateChange = false;
		}
	}
}

class CCheckBox extends CheckBox {
	//override attribute background = GC.panelBackground;
}

class CRadioButton extends RadioButton {
	//override attribute background = GC.panelBackground;
}
