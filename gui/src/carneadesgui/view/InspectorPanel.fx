/*
 * InspectorPanel.fx
 *
 * Created on 02.07.2009, 01:26:31
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

/**
* The statement inspector as used in the sidebar.
*/
class StatementInspector extends Inspector {

	// model statement object that is inspected
	public var statement: Statement = null;

	// id inspector
	var idTextBox: TextBox = InspectorTextBox { editable: false }
	var idLabel: Label = InspectorLabel { text: "id" }
	var idBox: HBox = HBox {
		content: [idLabel, idTextBox]
	}

	// content inspector
	var contentTextBox: TextBox = InspectorTextBox {
		editable: bind not controlsLocked
		action: function(): Void { control.changeStatementWff(statement, contentTextBox.text);}
	}
	var contentLabel: Label = InspectorLabel { text: "content" }
	var contentBox: HBox = HBox {
		content: [contentLabel, contentTextBox]
	}

	// status inspector
	var statusLabel: Label = InspectorLabel { text: "status" }
	var statusGroup: ToggleGroup = ToggleGroup {}
	var statusButtons: RadioButton[] = [for (s in statuses)
		InspectorRadioButton {
			text: s
			toggleGroup: statusGroup
			onMouseClicked: function(e: MouseEvent): Void {
				control.changeStatementStatus(statement, s);
			}
		}
	];
	var statusBox: HBox = HBox {
		content: [
			statusLabel,
			VBox { content: statusButtons }
		]
	}

	// acceptable?
	var acceptableLabel: Label = InspectorLabel { text: "acceptable" }
	var acceptableStatementCheckBox = InspectorCheckBox {
		text: "statement"
		disable: true
		defined: true
	}
	var acceptableComplementCheckBox = InspectorCheckBox {
		text: "complement"
		disable: true
		defined: true
	}
	var acceptableBox: HBox = HBox {
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
	var standardLabel: Label = InspectorLabel { text: "proof standard" }
	var standardGroup: ToggleGroup = ToggleGroup {}
	var standardButtons: RadioButton[] = [for (s in proofStandards)
		InspectorRadioButton {
			text: s
			toggleGroup: standardGroup
			onMouseClicked: function(e: MouseEvent): Void {
				control.changeStatementProofStandard(statement, s);
			}
		}
	];
	var standardBox: HBox = HBox {
		content: [
			standardLabel,
			VBox { content: standardButtons }
		]
	}

	override var content = [
		VBox {
			content: bind [
				idBox,
				contentBox,
				statusBox,
				acceptableBox,
				standardBox
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
			for (s in statusButtons) s.selected = { if (s.text == statement.status) true else false };
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
	var idTextBox: TextBox = InspectorTextBox { editable: false }
	var idLabel: Label = InspectorLabel { text: "id" }
	var idBox: HBox = HBox {
		content: [idLabel, idTextBox]
	}

	// title inspector
	var titleTextBox: TextBox = InspectorTextBox {
		editable: bind not controlsLocked
		action: function(): Void { control.changeArgumentTitle(argument, titleTextBox.text);}
	}
	var titleLabel: Label = InspectorLabel { text: "title" }
	var titleBox: HBox = HBox {
		content: [titleLabel, titleTextBox]
	}

	// defensible?
	var defensibleLabel: Label = InspectorLabel { text: "defensible" }
	var defensibleCheckBox = InspectorCheckBox {
		text: "defensible"
		disable: true
		defined: true
	}
	var defensibleBox: HBox = HBox {
		content: [
			defensibleLabel,
			VBox { content: [ defensibleCheckBox ] }
		]
	}

	// Scheme inspector
	var schemeTextBox: TextBox = InspectorTextBox {
		editable: bind not controlsLocked
		action: function(): Void { control.changeArgumentScheme(argument, schemeTextBox.text);}
	}
	var schemeLabel: Label = InspectorLabel { text: "scheme" }
	var schemeBox: HBox = HBox {
		content: [schemeLabel, schemeTextBox]
	}

	// argument direction inspector
	var directionLabel: Label = InspectorLabel { text: "direction" }
	var directionGroup: ToggleGroup = ToggleGroup {}
	var directionProButton: RadioButton = InspectorRadioButton {
		text: "pro"
		toggleGroup: directionGroup
		onMouseClicked: function(e: MouseEvent): Void {
			control.changeArgumentDirection(argument, "pro");
		}
	}
	var directionConButton: RadioButton = InspectorRadioButton {
		text: "con"
		toggleGroup: directionGroup
		onMouseClicked: function(e: MouseEvent): Void {
			control.changeArgumentDirection(argument, "con");
		}
	}
	var directionBox: HBox = HBox {
		content: [directionLabel, VBox {
				content: [directionProButton, directionConButton]
			}
		]
	}

	// weight slider
	var weightLabel: Label = InspectorLabel { text: "weight" }
	var weightSlider: InspectorSlider = InspectorSlider {
		min: 0.0
		max: 1.0
		action: function() {
			control.changeArgumentWeight(argument, weightSlider.value);
		}
	}
	var weightTextBox: TextBox = InspectorTextBox {
		editable: false
		text: bind "{weightSlider.value}"
		//action: function(): Void { control.changeArgumentScheme(argument, schemeTextBox.text);}
	}
	var weightBox: HBox = HBox {
		content: [weightLabel, VBox { content: [weightSlider, weightTextBox] }]
	}


	override var content = [
		VBox {
			content: bind [
				idBox,
				titleBox,
				defensibleBox,
				schemeBox,
				directionBox,
				weightBox,
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
    var roleTextBox: TextBox = InspectorTextBox {
	    editable: bind not controlsLocked
	    action: function(): Void { control.changePremiseRole(premise, roleTextBox.text);}
    }
    var roleLabel: Label = InspectorLabel { text: "role" }
    var roleBox: HBox = HBox {
	    content: [roleLabel, roleTextBox]
    }

    // exception inspector
    var exceptionLabel: Label = InspectorLabel { text: "exception" }
    var exceptionCheckBox: InspectorCheckBox = InspectorCheckBox {
	    text: "exception"
	    action: function(): Void { control.changePremiseType(premise, exceptionCheckBox.selected); }
    }
    var exceptionBox: HBox = HBox {
	    content: [exceptionLabel, exceptionCheckBox]
    }

    // negation inspector
    var negatedLabel: Label = InspectorLabel { text: "negated" }
    var negatedCheckBox: InspectorCheckBox = InspectorCheckBox {
	    text: "negate"
	    action: function(): Void { control.negatePremise(premise); }
    }
    var negatedBox: HBox = HBox {
	    content: [negatedLabel, negatedCheckBox]
    }

    override var content = [
	VBox {
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
	var titleTextBox: TextBox = InspectorTextBox {
		editable: true
		action: function(): Void { control.changeGraphTitle(graph, titleTextBox.text);}
	}
	var titleLabel: Label = InspectorLabel { text: "title" }
	var titleBox: HBox = HBox {
		content: [titleLabel, titleTextBox]
	}

	override var content = [
		VBox {
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
public class InspectorPanel extends Panel {
	public var control: CarneadesControl = null;
	public var constraintY: Number = bind listViewHeight + editButtonPanelHeight;
	public var mode: Integer = inspectorDefaultMode;

	override var layoutInfo = bind LayoutInfo {
		minWidth: bind inspectorPanelWidth;
		width: bind inspectorPanelWidth;
		// height constraints are currently switched off
		//minHeight: bind appHeight - constraintY - verticalWindowMismatch;
		//height: bind appHeight - constraintY - verticalWindowMismatch;
	}

	var statementInspector: StatementInspector = StatementInspector {
		control: bind control
		visible: bind (mode == inspectorStatementMode)
	}

	var argumentInspector: ArgumentInspector = ArgumentInspector {
		control: bind control
		visible: bind (mode == inspectorArgumentMode)
	}

	var premiseInspector: PremiseInspector = PremiseInspector {
		control: bind control
		visible: bind (mode == inspectorPremiseMode)
	}

	var graphInspector: GraphInspector = GraphInspector {
		control: bind control
		visible: bind (mode == inspectorGraphMode)
	}

	override var content = bind [
		/*LayoutRect {
			width: bind inspectorPanelWidth;
			height: bind appHeight - constraintY - verticalWindowMismatch;
			fill: panelBackground
		},*/
		graphInspector,
		statementInspector,
		argumentInspector,
		premiseInspector
	];

	public function editStatement(s: Statement): Void {
		statementInspector.statement = s;
		update(null);
	}

	public function editArgument(a: Argument): Void {
		argumentInspector.argument = a;
		update(null);
	}

	public function editPremise(pr: Premise): Void {
		premiseInspector.premise = pr;
		update(null);
	}

	public function editGraph(a: ArgumentGraph): Void {
		graphInspector.graph = a;
		update(null);
	}

	public function reset(): Void {
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
