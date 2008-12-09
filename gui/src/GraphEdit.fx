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
import Carneades.Graph.GC.*;
import Carneades.Graph.Elements.Elements.*;

// Abstract Controller Class for Interaction
import Carneades.Control.GraphControl;


public class GraphEdit extends SwingPanel {

	override var x = 0;
	override var y = 0;
	override var visible = true;

	public var control: GraphControl;
	public var argumentGraph: ArgumentGraph;

	 var statementPanel = StatementEditPanel { 
		control: bind control, argumentGraph: bind argumentGraph 
		preferredSize: bind this.preferredSize
	};
	 var argumentPanel = ArgumentEditPanel { 
		control: bind control, argumentGraph: bind argumentGraph 
		preferredSize: bind this.preferredSize
	};
	 var premisePanel = PremiseEditPanel {
		control: bind control, argumentGraph: bind argumentGraph 
		preferredSize: bind this.preferredSize
	};
	 var graphPanel = GraphEditPanel {
		control: bind control, argumentGraph: bind argumentGraph 
		preferredSize: bind this.preferredSize
	};

	 var panelEmpty = true;
	override var content = bind { if (not panelEmpty) [graphPanel, statementPanel, argumentPanel, premisePanel] else []};

	public function update(): Void {
		panelEmpty = false;
		if (control.getSelectedModel() != []) {
			if (control.getSelectedModel()[0] instanceof Argument) {
				statementPanel.visible = false;
				argumentPanel.visible = true;
				premisePanel.visible = false;
				graphPanel.visible = false;
	
				var selected = control.getSelectedModel() [0];
				argumentPanel.loadArgument(selected as Argument);
			}
			else if (control.getSelectedModel()[0] instanceof Statement) {
	
				statementPanel.visible = true;
				argumentPanel.visible = false;
				premisePanel.visible = false;
				graphPanel.visible = false;
	
				statementPanel.loadStatement(control.getSelectedModel() [0] as Statement);
			}
			else if (control.getSelectedModel()[0] instanceof Premise) {
				statementPanel.visible = false;
				argumentPanel.visible = false;
				premisePanel.visible = true;
				graphPanel.visible = false;
	
				premisePanel.loadPremise(control.getSelectedModel() [0] as Premise);
			} else if (control.getSelectedModel()[0] instanceof ArgumentGraph) {
				statementPanel.visible = false;
				argumentPanel.visible = false;
				premisePanel.visible = false;
				graphPanel.visible = true;

				graphPanel.loadGraph(control.getSelectedModel() [0] as ArgumentGraph);
			}
		} else {
			panelEmpty = true;
		}
	}
}

public class EditPanel extends FlowPanel {
	//override var background = panelBackground;
	override var alignment = HorizontalAlignment.LEFT;
	override var width = editWidth;
	override var height = editHeight;
	override var visible = false;
	protected var control: GraphControl;
	public var argumentGraph: ArgumentGraph;
	protected var editComponentWidth = bind editWidth - editLabelWidth - 20;

}

public class StatementEditPanel extends EditPanel {
	var statement: Statement;
	
	// General Components

	 var idField: IdField = IdField {
		editable: bind idsEditable
		preferredSize: [editComponentWidth, textFieldHeight ]
		action: function(): Void {
			control.changeStatementId(statement, idField.text);
		}
	}
	
	 var contentField: ContentField = ContentField {
		preferredSize: [ editComponentWidth, textFieldHeight ]
		action: function(): Void {
			control.changeStatementWff(statement, contentField.text);
		}
	}

	 var acceptableBox: SwingCheckBox = CCheckBox {
		text: "statement"
		enabled: false
		preferredSize: [ editWidth - editLabelWidth - 30, 20 ]
		selected: bind statement.ok
	}

	 var compAcceptableBox: SwingCheckBox = CCheckBox {
		text: "complement"
		enabled: false
		preferredSize: [ editWidth - editLabelWidth - 30, 20 ]
		selected: bind statement.complementOk
	}

	// Proof Standard Components

	 var proofStandardBox: ProofStandardField = ProofStandardField {
		preferredSize: [ editComponentWidth - 30, 20 ]
		action: function(): Void {
			submitStandard();
		}
	}

	// temporary function to submit a new Proof Standard
	 function submitStandard(): Void {
		control.changeStatementProofStandard( statement,
											  selectedStandard);
	}

	 var standardGroup: SwingToggleGroup = SwingToggleGroup {};

	 var selectedStandard: String = bind (// if (BAButton.selected) "BA" 
														if (SEButton.selected) "SE" 
														else if (DVButton.selected) "DV" 
														else if (BRDButton.selected) "BRD" 
														else if (CCEButton.selected) "CCE" 
														else "PE");

	/* 
	 var BAButton: SwingRadioButton = CRadioButton {
		toggleGroup: standardGroup
		text: "best argument"
		preferredSize: [ editComponentWidth, 20 ]
		action: function(): Void {
			submitStandard();
		}
	}
	*/

	 var SEButton: SwingRadioButton = CRadioButton {
		toggleGroup: standardGroup
		text: "scintilla of evidence"
		preferredSize: [ editComponentWidth, 20 ]
		action: function(): Void {
			submitStandard();
		}
	}

	 var DVButton: SwingRadioButton = CRadioButton {
		toggleGroup: standardGroup
		text: "dialectical validity"
		preferredSize: [ editComponentWidth, 20 ]
		action: function(): Void {
			submitStandard();
		}
	}

	 var PEButton: SwingRadioButton = CRadioButton {
		toggleGroup: standardGroup
		text: "preponderance of evidence"
		preferredSize: [ editComponentWidth, 20 ]
		action: function(): Void {
			submitStandard();
		}
	}

	 var BRDButton: SwingRadioButton = CRadioButton {
		toggleGroup: standardGroup
		text: "beyond reasonable doubt"
		preferredSize: [ editComponentWidth, 20 ]
		action: function(): Void {
			submitStandard();
		}
	}

	 var CCEButton: SwingRadioButton = CRadioButton {
		toggleGroup: standardGroup
		text: "clear & convincing evidence"
		preferredSize: [ editComponentWidth, 20 ]
		action: function(): Void {
			submitStandard();
		}
	}

	// Status Components
	
	 var statusGroup: SwingToggleGroup = SwingToggleGroup {};

	 var statedButton: SwingRadioButton = CRadioButton {
		text: "stated"
		toggleGroup: statusGroup
		preferredSize: [ editComponentWidth, 20 ]
		action: function() {
			control.changeStatementStatus(statement, statedButton.text);
		}
	}
	 var questionedButton: SwingRadioButton = CRadioButton {
		text: "questioned"
		toggleGroup: statusGroup
		preferredSize: [ editComponentWidth, 20 ]
		action: function() {
			control.changeStatementStatus(statement, questionedButton.text);
		}
	}
	 var assumedTrueButton: SwingRadioButton = CRadioButton {
		text: "assumed true"
		toggleGroup: statusGroup
		preferredSize: [ editComponentWidth, 20 ]
		action: function() {
			control.changeStatementStatus(statement, assumedTrueButton.text);
		}
	}
	 var assumedFalseButton: SwingRadioButton = CRadioButton {
		text: "assumed false"
		toggleGroup: statusGroup
		preferredSize: [ editComponentWidth, 20 ]
		action: function() {
			control.changeStatementStatus(statement, assumedFalseButton.text);
		}
	}
	 var acceptedButton: SwingRadioButton = CRadioButton {
		text: "accepted"
		toggleGroup: statusGroup
		preferredSize: [ editComponentWidth, 20 ]
		action: function() {
			control.changeStatementStatus(statement, acceptedButton.text);
		}
	}
	 var rejectedButton: SwingRadioButton = CRadioButton {
		text: "rejected"
		toggleGroup: statusGroup
		preferredSize: [ editComponentWidth, 20 ]
		action: function() {
			control.changeStatementStatus(statement, rejectedButton.text);
		}
	}

	override var content = bind [ 
										Label { text: "id ", preferredSize: [editLabelWidth, 20] }, idField, 
										Label { text: "content ", preferredSize: [editLabelWidth, 20] }, contentField,
										Label { text: "status ", preferredSize: [editLabelWidth, 20] }, statedButton, 
										Label { text: "", preferredSize: [editLabelWidth, 20] }, questionedButton, 
										Label { text: "", preferredSize: [editLabelWidth, 20] }, assumedTrueButton, 
										Label { text: "", preferredSize: [editLabelWidth, 20] }, assumedFalseButton, 
										Label { text: "", preferredSize: [editLabelWidth, 20] }, acceptedButton, 
										Label { text: "", preferredSize: [editLabelWidth, 20] }, rejectedButton, 
										Label { text: "acceptable ", preferredSize: [editLabelWidth, 20] }, acceptableBox,
										Label { text: "", preferredSize: [editLabelWidth, 20] }, compAcceptableBox,
										Label { text: "proof standard", preferredSize: [editLabelWidth, 20] }, SEButton,
										Label { text: "", preferredSize: [editLabelWidth, 20] }, DVButton, 
										// Label { text: "", preferredSize: [editLabelWidth, 20] }, BAButton, 
										Label { text: "", preferredSize: [editLabelWidth, 20] }, PEButton, 
										Label { text: "", preferredSize: [editLabelWidth, 20] }, CCEButton, 
										Label { text: "", preferredSize: [editLabelWidth, 20] }, BRDButton, 
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
		else if (statement.standard instanceof BeyondReasonableDoubt) { BRDButton.selected = true; }
		else if (statement.standard instanceof ClearAndConvincingEvidence) { CCEButton.selected = true; }
	    else /* if (statement.standard instanceof Preponderance) */ { PEButton.selected = true; }
		// else /*if (statement.standard instanceof BestArgument)*/ { BAButton.selected = true; }
		
	}
}

public class ArgumentEditPanel extends EditPanel {
	var argument: Argument;

	// Components

	public var idField: IdField = IdField {
		preferredSize: [ editComponentWidth, textFieldHeight ]
		editable: bind idsEditable
		action: function(): Void {
			control.changeArgumentId(argument, idField.text);
		}
	}
	
	 var titleField: ContentField = ContentField {
		preferredSize: [ editComponentWidth, textFieldHeight ]
		action: function(): Void {
			control.changeArgumentTitle(titleField.text, argument);
		}
	}

	public var defensibleBox: SwingCheckBox = CCheckBox {
		preferredSize: [ editComponentWidth, 20 ]
		enabled: false
		selected: bind argument.ok
	}

	 var schemeField: SchemeField = SchemeField {
		preferredSize: [ editComponentWidth, textFieldHeight ]
		action: function(): Void {
			control.changeArgumentScheme(argument, schemeField.text);
		}
	}

	 var directionGroup: SwingToggleGroup = SwingToggleGroup {};

	 var proButton: SwingRadioButton = CRadioButton {
		preferredSize: [editComponentWidth / 2, 20]
		toggleGroup: directionGroup
		text: "pro"
		action: function(): Void {
			control.changeArgumentDirection(argument, "pro");
		}
	}

	 var conButton: SwingRadioButton = CRadioButton {
		preferredSize: [editComponentWidth / 2, 20]
		toggleGroup: directionGroup
		text: "con"
		action: function(): Void {
			control.changeArgumentDirection(argument, "con");
		}
	}

	 var weightSlider: WeightSlider = WeightSlider {
		argument: bind argument
		control: bind control
		preferredSize: [ editComponentWidth-45, 20 ]
		maximum: 100
		minimum: 0
	}

	 var weightNumber: SwingTextField = SwingTextField {
		preferredSize: [ 40, textFieldHeight ]
		editable: false
		text: bind ".{(weightSlider.value).toString()}"
	}

	override var content = bind [ 
										Label { text: "id " preferredSize: [editLabelWidth, 20]}, idField, 
										Label { text: "title " preferredSize: [editLabelWidth, 20]}, titleField, 
										Label { text: "direction ", preferredSize: [editLabelWidth, 20]}, proButton, conButton,
										Label { text: "premises hold", preferredSize: [editLabelWidth, 20]}, defensibleBox,
										Label { text: "weight ", preferredSize: [editLabelWidth, 20]}, weightSlider, weightNumber,
										Label { text: "scheme ", preferredSize: [editLabelWidth, 20] }, schemeField,
										];

	// Functions

	public function loadArgument(a: Argument): Void {
		argument = a;
		idField.text = argument.id;
		titleField.text = argument.title;

		if (a.pro) { 
			proButton.selected = true;
			conButton.selected = false;
		} else {
			proButton.selected = false;
			conButton.selected = true;
		}

		weightSlider.setValue((argument.weight * 100) as Integer);
	}
}

public class PremiseEditPanel extends EditPanel {
	var premise: Premise;

	// Components
	public var roleField: RoleField = RoleField {
		preferredSize: [ editComponentWidth, textFieldHeight ]
		editable: true
		action: function(): Void {
			control.changePremiseRole(premise, roleField.text);
		}
	}

	public var exceptionBox: SwingCheckBox = CCheckBox {
		preferredSize: [ editComponentWidth, 20 ]
		selected: premise.exception
		action: function(): Void {
			control.changePremiseType(premise, exceptionBox.selected);
		}
	}

	public var negationBox: SwingCheckBox = CCheckBox {
		preferredSize: [ editComponentWidth, 20 ]
		selected: premise.negative
		action: function(): Void {
			control.negatePremise(premise);
		}
	}

	override var content = bind [ 
										Label { text: "role ", preferredSize: [editLabelWidth, 20] }, roleField, 
										Label { text: "exception ", preferredSize: [editLabelWidth, 20] }, exceptionBox,
										Label { text: "negated ", preferredSize: [editLabelWidth, 20] }, negationBox,
										];

	public function loadPremise(p: Premise): Void {
		premise = p;
		roleField.text = p.role;
		negationBox.selected = p.negative;
		exceptionBox.selected = p.exception;
	}
}

public class GraphEditPanel extends EditPanel {

	// Components
	 var idField: IdField = IdField {
		editable: bind idsEditable
		preferredSize: [editComponentWidth, textFieldHeight ]
	}
	
	 var titleField: ContentField = ContentField {
		preferredSize: [ editComponentWidth, textFieldHeight ]
		action: function(): Void {
			control.changeGraphTitle(titleField.text, argumentGraph);
		}
	}

	override var content = bind [ 
										Label { text: "id ", preferredSize: [editLabelWidth, 20] }, idField, 
										Label { text: "title ", preferredSize: [editLabelWidth, 20] }, titleField,
										];

	public function loadGraph(g: ArgumentGraph): Void {
		argumentGraph = g;
		idField.text = g.id;
		titleField.text = g.title;
	}
}

// INTERMEDIATE COMPONENT CLASSES

class IdField extends SwingTextField {
	override var visible = true;
	override var editable = false;
	override var preferredSize = [editWidth - 50, 20];
}

class SchemeField extends SwingTextField {
	override var visible = true;
	override var preferredSize = [editWidth - 50, 20];
}

class ContentField extends SwingTextField {
	override var editable = true;
	override var preferredSize = [editWidth - 80, 20];
}

class RoleField extends SwingTextField {
	override var editable = false;
	override var preferredSize = [editWidth - 40, 20];
}

class BooleanBox extends ComboBox {
	public var value: Boolean;
	override var visible = true;
	override var items = [
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
	public var pro: Boolean;
	override var visible = true;
	override var items = [
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
	public var standard: ProofStandard;
	override var visible = true;
	override var items = [
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
	public var statement: Statement;
	override var visible = true;
	override var items = [
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
	public var premiseType: String = "";
	override var visible = true;
	override var items = [
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

// Temporary Textfield components until the combobox gets an action var

class LimitedTextField extends SwingTextField {
	var choices: String[] = [];
	override var foreground = bind { if (sizeof choices[c | c == this.text] == 1) Color.DARKGREEN else Color.DARKRED};
	var verified: Boolean = bind { (sizeof choices[c | c == this.text] == 1) };
}

class BooleanField extends LimitedTextField {
	override var preferredSize = [70, 20];
	override var choices = [ "true", "false" ];
}

class ProofStandardField extends LimitedTextField {
	override var preferredSize = [80, 20];
	override var choices = [ "SE", "DV", "BA" ];
}

class PremiseField extends LimitedTextField {
	override var preferredSize = [100, 20];
	override var choices = [ "ordinary", "exception" ];
}

class DirectionField extends LimitedTextField {
	override var preferredSize = [100, 20];
	override var choices = [ "pro", "con" ];
}

class WeightSlider extends SwingSlider {
	public var argument: Argument = Argument {} on replace { value = (argument.weight * 100) as Integer; }
	 var updateChange: Boolean = true; // needs to be true to avoid inital command dispatch from on replace value
	override var value = (argument.weight) as Integer on replace { submitWeight(); }
	public var control: GraphControl;

	function setValue(v: Integer) {
		updateChange = true;
		value = v;
	}

	function submitWeight(): Void {
		if (not updateChange) { 
			control.changeArgumentWeight(argument, (value as Number) / 100); 
		} else {
			updateChange = false;
		}
	}
}

class CCheckBox extends SwingCheckBox {
	//override var background = panelBackground;
}

class CRadioButton extends SwingRadioButton {
	//override var background = panelBackground;
}
