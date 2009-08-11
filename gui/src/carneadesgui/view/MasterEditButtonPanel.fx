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

import javafx.scene.layout.Panel;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;
import javafx.scene.layout.LayoutInfo;
import javafx.scene.control.Button;

import carneadesgui.GC.*;
import carneadesgui.control.CarneadesControl;
import carneadesgui.model.Argument.*;
import carneadesgui.model.Argument;


var inspectorLayoutInfo: LayoutInfo = LayoutInfo {
		minWidth: bind inspectorPanelWidth;
		width: bind inspectorPanelWidth;
		minHeight: bind editButtonPanelHeight;
		height: bind editButtonPanelHeight;
}

class EditPanelButton extends Button {}

abstract class EditButtonPanel extends Panel {
	public-init var control: CarneadesControl;
	override var layoutInfo = inspectorLayoutInfo;
}

class DefaultEditButtonPanel extends EditButtonPanel {
	override var content = bind HBox {
		content: [
			EditPanelButton {
				text: "add graph"
				action: function() {
					control.addArgumentGraph(control.defaultArgumentGraph(control.getNewGraphId()));
				}
			},
			EditPanelButton {
				text: "add statement"
				action: function() {
				control.addStatement();
				}
			}
		]
	}
}

class GraphEditButtonPanel extends EditButtonPanel {
	public var graph: ArgumentGraph = null;
	override var content = bind HBox {
		content: [
			EditPanelButton {
				text: "add graph"
				action: function() {
					control.addArgumentGraph(control.defaultArgumentGraph(control.getNewGraphId()));
				}
			},
			EditPanelButton {
				text: "delete graph"
				action: function() { control.removeCurrentArgumentGraph(); }
			}
		]
	}
}

class StatementEditButtonPanel extends EditButtonPanel {
	public var statement: Statement = null;
	override var content = bind HBox {
		content: [
			EditPanelButton {
				text: "remove statement"
				action: function() {
					control.removeStatementFromBox(null);
				}
			},
			EditPanelButton {
				text: "add argument"
				action: function() {
					control.addArgumentToSelected();
				}
			}
		]
	}
}

class ArgumentEditButtonPanel extends EditButtonPanel {
	public var argument: Argument = null;
	override var content = bind HBox {
	content: [
			EditPanelButton {
				text: "remove argument"
				action: function() {
					control.removeArgumentFromBox(null);
				}
			},
			EditPanelButton {
				text: "add premise"
				action: function() {
					control.addPremiseToSelected();
				}
			}
		]
	}
}

class PremiseEditButtonPanel extends EditButtonPanel {
	public var premise: Premise= null;
}

public class MasterEditButtonPanel extends Panel {
	public var control: CarneadesControl = null;
	public var mode: Integer = inspectorDefaultMode;
	override var layoutInfo = inspectorLayoutInfo;

	var defaultEditButtonPanel: DefaultEditButtonPanel = DefaultEditButtonPanel {
		control: bind control
		visible: bind (mode == inspectorDefaultMode)
	}

	var statementEditButtonPanel: StatementEditButtonPanel = StatementEditButtonPanel {
		control: bind control
		visible: bind (mode == inspectorStatementMode)
	}
	var argumentEditButtonPanel: ArgumentEditButtonPanel = ArgumentEditButtonPanel {
		control: bind control
		visible: bind (mode == inspectorArgumentMode)
	}

	var premiseEditButtonPanel: PremiseEditButtonPanel = PremiseEditButtonPanel {
		control: bind control
		visible: bind (mode == inspectorPremiseMode)
	}

	var graphEditButtonPanel: GraphEditButtonPanel = GraphEditButtonPanel {
		control: bind control
		visible: bind (mode == inspectorGraphMode)
	}

	override var content = bind [
		LayoutRect {
			width: bind inspectorPanelWidth;
			fill: panelBackground
		},
		defaultEditButtonPanel,
		graphEditButtonPanel,
		statementEditButtonPanel,
		argumentEditButtonPanel,
		premiseEditButtonPanel,
	];

	public function editStatement(s: Statement): Void {
		statementEditButtonPanel.statement = s;
		mode = inspectorStatementMode;
		update();
	}

	public function editArgument(a: Argument): Void {
		argumentEditButtonPanel.argument = a;
		mode = inspectorArgumentMode;
		update();
	}

	public function editPremise(pr: Premise): Void {
		premiseEditButtonPanel.premise = pr;
		mode = inspectorPremiseMode;
		update();
	}

	public function editGraphs(ag: ArgumentGraph): Void {
		graphEditButtonPanel.graph = ag;
		mode = inspectorGraphMode;
		update();
	}

	public function reset(): Void {
		mode = inspectorDefaultMode;
		update();
	}

	/**
	* General update function. Assumes that mode has been set before.
	*/
	public function update(): Void {
	}
}



