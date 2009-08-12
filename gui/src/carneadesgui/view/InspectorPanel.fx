/*
Carneades Argumentation Library and Tools.
Copyright (C) 2008 Thomas Gordon and Matthias Grabmair

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

import carneadesgui.GC.*;
import carneadesgui.control.CarneadesControl;
import javafx.scene.input.MouseEvent;
import javafx.scene.control.Label;
import javafx.scene.control.TextBox;
import javafx.scene.control.ToggleGroup;
import javafx.scene.control.RadioButton;
import javafx.scene.layout.HBox;
import javafx.scene.layout.LayoutInfo;
import javafx.scene.layout.Panel;
import javafx.scene.layout.VBox;
import carneadesgui.model.Argument;
import carneadesgui.model.Argument.*;
import carneadesgui.view.InspectorElements.*;
import carneadesgui.view.GraphUpdate;

import javafx.scene.paint.Color;

/**
* The statement inspector as used in the sidebar.
*/
class StatementInspector extends Inspector {

	// model statement object that is inspected
	public var statement: Statement = null;

	// id inspector
	def idTextBox: TextBox = InspectorTextBox {
		editable: bind idsEditable
		action: function(): Void { control.changeStatementId(statement, idTextBox.text); }
	}
	def idLabel: Label = InspectorLabel { text: "id" }
	def idBox: HBox = HBox {
		content: [idLabel, idTextBox]
	}

	// content inspector
	def contentTextBox: TextBox = InspectorTextBox {
		editable: bind not controlsLocked
		action: function(): Void { control.changeStatementWff(statement, contentTextBox.text);}
	}
	def contentLabel: Label = InspectorLabel { text: "content" }
	def contentBox: HBox = HBox {
		content: [contentLabel, contentTextBox]
	}

	// status inspector
	def statusLabel: Label = InspectorLabel { text: "status" }
	def statusGroup: ToggleGroup = ToggleGroup {}
	def issueButton = InspectorRadioButton {
		text: "issue"
		toggleGroup: statusGroup
		onMouseClicked: function(e: MouseEvent): Void {
			control.changeStatementStatus(statement, "stated");
		}
	}
	def trueButton = InspectorRadioButton {
		text: "true"
		toggleGroup: statusGroup
		onMouseClicked: function(e: MouseEvent): Void {
			control.changeStatementStatus(statement, "assumed true");
		}
	}
	def falseButton = InspectorRadioButton {
		text: "false"
		toggleGroup: statusGroup
		onMouseClicked: function(e: MouseEvent): Void {
			control.changeStatementStatus(statement, "assumed false");
		}
	}
	
	def statusBox: HBox = HBox {
		content: [
			statusLabel, VBox { content: [ issueButton, trueButton, falseButton ] }
		]
	}

	// acceptable?
	def acceptableLabel: Label = InspectorLabel { text: "acceptable" }
	def acceptableStatementCheckBox = InspectorCheckBox {
		text: "statement"
		disable: true
		defined: true
	}
	def acceptableComplementCheckBox = InspectorCheckBox {
		text: "complement"
		disable: true
		defined: true
	}
	def acceptableBox: HBox = HBox {
		content: [
			acceptableLabel,
			VBox { content: [
				acceptableStatementCheckBox,
				acceptableComplementCheckBox
				]
			}
		]
	}

	// proof standard
	def standardLabel: Label = InspectorLabel { text: "proof standard" }
	def standardGroup: ToggleGroup = ToggleGroup {}
	def standardButtons: RadioButton[] = [for (s in proofStandards)
		InspectorRadioButton {
			text: s
			toggleGroup: standardGroup
			onMouseClicked: function(e: MouseEvent): Void {
				control.changeStatementProofStandard(statement, s);
			}
		}
	];
	def standardBox: HBox = HBox {
		content: [
			standardLabel,
			VBox { content: standardButtons }
		]
	}

	override def content = [
		VBox {
			spacing: INSPECTOR_PANEL_SPACING
			content: bind [
				idBox,
				contentBox,
				statusBox,
				standardBox,
				acceptableBox,
			]
		}
	];

	/**
	* Update the inspector from the selected model statement.
	*/
	override function update(u: GraphUpdate) {
		if (statement != null) {
			idTextBox.text = statement.id;
			contentTextBox.text = statement.wff;
			issueButton.selected = statement.stated() or statement.questioned();
			trueButton.selected = statement.assumedTrue() or statement.accepted();
			falseButton.selected = statement.assumedFalse() or statement.rejected();
			for (s in standardButtons) s.selected = { if (s.text == statement.getStandard()) true else false};
			acceptableStatementCheckBox.selected =  { if (statement.ok) true else false };
			acceptableComplementCheckBox.selected =  { if (statement.complementOk) true else false };
		} else reset("statement");
	}
}

/**
* The Argument inspector as used in the sidebar.
*/
class ArgumentInspector extends Inspector {
	public var argument: Argument = null;

	// id inspector
	def idTextBox: TextBox = InspectorTextBox {
		editable: bind idsEditable
		action: function(): Void { control.changeArgumentId(argument, idTextBox.text); }
	}
	def idLabel: Label = InspectorLabel { text: "id" }
	def idBox: HBox = HBox {
		content: [idLabel, idTextBox]
	}

	// title inspector
	def titleTextBox: TextBox = InspectorTextBox {
		editable: bind not controlsLocked
		action: function(): Void { control.changeArgumentTitle(argument, titleTextBox.text);}
	}
	def titleLabel: Label = InspectorLabel { text: "title" }
	def titleBox: HBox = HBox {
		content: [titleLabel, titleTextBox]
	}

	// defensible?
	def defensibleLabel: Label = InspectorLabel { text: "applicable" }
	def defensibleCheckBox = InspectorCheckBox {
		text: ""
		disable: true
		defined: true
	}
	def defensibleBox: HBox = HBox {
		content: [
			defensibleLabel,
			VBox { content: [ defensibleCheckBox ] }
		]
	}

	// Scheme inspector
	def schemeTextBox: TextBox = InspectorTextBox {
		editable: bind not controlsLocked
		action: function(): Void { control.changeArgumentScheme(argument, schemeTextBox.text);}
	}
	def schemeLabel: Label = InspectorLabel { text: "scheme" }
	def schemeBox: HBox = HBox {
		content: [schemeLabel, schemeTextBox]
	}

	// argument direction inspector
	var directionLabel: Label = InspectorLabel { text: "direction" }
	def directionGroup: ToggleGroup = ToggleGroup {}
	def directionProButton: RadioButton = InspectorRadioButton {
		text: "pro"
		toggleGroup: directionGroup
		onMouseClicked: function(e: MouseEvent): Void {
			control.changeArgumentDirection(argument, "pro");
		}
	}
	def directionConButton: RadioButton = InspectorRadioButton {
		text: "con"
		toggleGroup: directionGroup
		onMouseClicked: function(e: MouseEvent): Void {
			control.changeArgumentDirection(argument, "con");
		}
	}
	def directionBox: HBox = HBox {
		content: [directionLabel, VBox {
				content: [directionProButton, directionConButton]
			}
		]
	}

	// weight slider
	def weightLabel: Label = InspectorLabel { text: "weight" }
	def weightSlider: InspectorSlider = InspectorSlider {
		min: 0.0
		max: 1.0
		action: function() {
			control.changeArgumentWeight(argument, weightSlider.value);
		}
	}
	def weightTextBox: TextBox = InspectorTextBox {
		editable: false
		text: bind { 
		if (("{weightSlider.value}").length() <= 4)
			"{weightSlider.value}"
			else ("{weightSlider.value}").substring(0, 4)
		}
	}
	def weightBox: HBox = HBox {
		content: [weightLabel, VBox { content: [weightSlider, weightTextBox] }]
	}


	override def content = [
		VBox {
			spacing: INSPECTOR_PANEL_SPACING
			content: bind [
				idBox,
				titleBox,
				schemeBox,
				directionBox,
				weightBox,
				defensibleBox,
			]
		}
	];

	/**
	* Update the inspector from the selected model argument.
	*/
	override function update(u: GraphUpdate) {
		if (argument != null) {
			idTextBox.text = argument.id;
			titleTextBox.text = argument.title;
			schemeTextBox.text = argument.scheme.id;
			defensibleCheckBox.selected = {if (argument.ok) true else false };
			directionProButton.selected = { if (argument.pro) true else false };
			directionConButton.selected = { if (not argument.pro) true else false };
			weightSlider.setValue(argument.weight);
		} else reset("argument");
	}
}

/**
* The Premise inspector as used in the sidebar.
*/
class PremiseInspector extends Inspector {
    public var premise: Premise = null;

    // Role inspector
    def roleTextBox: TextBox = InspectorTextBox {
	    editable: bind not controlsLocked
	    action: function(): Void { control.changePremiseRole(premise, roleTextBox.text);}
    }
    def roleLabel: Label = InspectorLabel { text: "role" }
    def roleBox: HBox = HBox {
	    content: [roleLabel, roleTextBox]
    }

    // exception inspector
    def exceptionLabel: Label = InspectorLabel { text: "exception" }
    def exceptionCheckBox: InspectorCheckBox = InspectorCheckBox {
	    text: ""
	    action: function(): Void { control.changePremiseType(premise, exceptionCheckBox.selected); }
    }
    def exceptionBox: HBox = HBox {
	    content: [exceptionLabel, exceptionCheckBox]
    }

    // negation inspector
    def negatedLabel: Label = InspectorLabel { text: "negated" }
    def negatedCheckBox: InspectorCheckBox = InspectorCheckBox {
	    text: ""
	    action: function(): Void { control.negatePremise(premise); }
    }
    def negatedBox: HBox = HBox {
	    content: [negatedLabel, negatedCheckBox]
    }

    override def content = [
		VBox {
			spacing: INSPECTOR_PANEL_SPACING
			content: bind [
				roleBox,
				exceptionBox,
				negatedBox
			]
		}
    ];

    /**
    * Update the inspector from the selected model premise.
    */
    override function update(u: GraphUpdate) {
		if (premise != null) {
			roleTextBox.text = premise.role;
			exceptionCheckBox.selected = premise.exception;
			negatedCheckBox.selected = premise.negative;
		} else reset("premise");
    }
}

/**
* The Premise inspector as used in the sidebar.
*/
class GraphInspector extends Inspector {
	public var graph: ArgumentGraph = null;

	// id inspector
	def titleTextBox: TextBox = InspectorTextBox {
		editable: true
		action: function(): Void { control.changeGraphTitle(graph, titleTextBox.text);}
	}
	def titleLabel: Label = InspectorLabel { text: "title" }
	def titleBox: HBox = HBox {
		content: [titleLabel, titleTextBox]
	}

	override def content = [
		VBox {
			spacing: INSPECTOR_PANEL_SPACING
			content: bind [
				titleBox,
			]
		}
	];

	/**
	* Update the inspector from the selected model graph.
	*/
	override function update(u: GraphUpdate) {
		if (graph != null) {
			titleTextBox.text = graph.title;
		} else reset("graph");
	}
}

/**
* Panel containing the inspector components.
*/
public class InspectorPanel extends MoveablePanel {
	override def title = "Element Inspector";
	override def width = inspectorPanelWidth;
	override def height = INSPECTOR_PANEL_HEIGHT;
	override def padding = INSPECTOR_PADDING;
	public var control: CarneadesControl;
	public var view: CarneadesView;
	public def mode: Integer = bind view.mode;
	override var display = false;
	override def onClose = function() {
		hide();
	}

	def statementInspector: StatementInspector = StatementInspector {
		control: bind control
	}

	def argumentInspector: ArgumentInspector = ArgumentInspector {
		control: bind control
	}

	def premiseInspector: PremiseInspector = PremiseInspector {
		control: bind control
	}

	def graphInspector: GraphInspector = GraphInspector {
		control: bind control
	}

	override def content = bind [
		if (mode == inspectorGraphMode) graphInspector else null,
		if (mode == inspectorStatementMode) statementInspector else null,
		if (mode == inspectorArgumentMode) argumentInspector else null,
		if (mode == inspectorPremiseMode) premiseInspector else null
	];

	public function editStatement(s: Statement): Void {
		statementInspector.statement = s;
		if (not display) show();
		update(null);
	}

	public function editArgument(a: Argument): Void {
		argumentInspector.argument = a;
		if (not display) show();
		update(null);
	}

	public function editPremise(pr: Premise): Void {
		premiseInspector.premise = pr;
		if (not display) show();
		update(null);
	}

	public function editGraph(a: ArgumentGraph): Void {
		graphInspector.graph = a;
		if (not display) show();
		update(null);
	}

	public function reset(): Void {
		hide();
		update(null);
	}

	/**
	* General update function. Assumes that mode has been set before.
	*/
	public function update(u: GraphUpdate): Void {
		if (mode == inspectorStatementMode) statementInspector.update(null)
		else if (mode == inspectorArgumentMode) argumentInspector.update(null)
		else if (mode == inspectorGraphMode) graphInspector.update(null)
		else if (mode == inspectorPremiseMode) premiseInspector.update(null);
	}
}
