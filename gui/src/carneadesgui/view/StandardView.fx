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

import javafx.scene.paint.Color;

// general imports
import javafx.scene.Scene;
import javafx.scene.layout.LayoutInfo;
import javafx.scene.layout.VBox;
import javafx.stage.Stage;
import javafx.stage.Alert;

// control imports
import carneadesgui.GC.*;

// model imports
import carneadesgui.model.Argument;
import carneadesgui.model.Argument.*;

// view imports
import carneadesgui.view.InspectorPanel;
import carneadesgui.view.GraphListView;
import carneadesgui.view.ToolBar;
import carneadesgui.view.GraphUpdate;
import javafx.scene.layout.Stack;
import javafx.geometry.HPos;
import javafx.geometry.VPos;


/**
* The standard GUI view that is currently used.
*/
public class StandardView extends CarneadesView {

	def graphPanel: GraphPanel = GraphPanel {
		//constraintX: inspectorPanelWidth + mainPanelSpacing
		constraintY: toolBarHeight
		graph: bind currentGraph
		control: bind control
	}

	def graphListView: GraphListView = GraphListView {
		control: bind control
		view: bind this
		x: appWidth - inspectorPanelWidth - INSPECTOR_WINDOWEDGE_PADDING 
		y: INSPECTOR_WINDOWEDGE_PADDING
		maxX: bind appWidth - inspectorPanelWidth - INSPECTOR_WINDOWEDGE_PADDING
		minX: 0
		maxY: bind appHeight - toolBarHeight - GRAPHLISTVIEW_HEIGHT - INSPECTOR_WINDOWEDGE_PADDING
		minY: 0
	}

	def inspectorPanel: InspectorPanel = InspectorPanel {
		control: bind control
		view: this
		x: appWidth - inspectorPanelWidth - INSPECTOR_WINDOWEDGE_PADDING
		y: 2 * INSPECTOR_WINDOWEDGE_PADDING + GRAPHLISTVIEW_HEIGHT
		maxX: bind appWidth - inspectorPanelWidth - INSPECTOR_WINDOWEDGE_PADDING
		minX: 0
		maxY: bind appHeight - toolBarHeight - GRAPHLISTVIEW_HEIGHT - INSPECTOR_WINDOWEDGE_PADDING
		minY: 0
	}

	def toolBar: ToolBar = ToolBar {
		control: bind control
		view: bind this
	}

	override function update(u: GraphUpdate) {
		if (u.changedAttribute or u.selection or u.listSelection)
			inspectorPanel.update(u);
		if (u.selection or u.graphSelection or u.listView)
			graphListView.update(u);
	}

	override function displayGraphListView(){
		if (not graphListView.display) graphListView.show();
	}

	override function isVisible(e: GraphElement) {
	    graphPanel.isVisibleInGraphPanel(e);
	}

	override function focusOn(e: GraphElement) {
		graphPanel.focusOn(e);
	}

	override function editStatement(s: Statement) {
		mode = inspectorStatementMode;
		inspectorPanel.editStatement(s);
	}

	override function editArgument(a: Argument) {
		mode = inspectorArgumentMode;
		inspectorPanel.editArgument(a);
	}

	override function editPremise(p: Premise) {
		mode = inspectorPremiseMode;
		inspectorPanel.editPremise(p);
	}

	override function editGraph(a: ArgumentGraph) {
		mode = inspectorGraphMode;
		inspectorPanel.editGraph(a);
	}

	override function editNothing() {
		inspectorPanel.reset();
	}

	override function alert(t: String) {
		Alert.inform(t);
	}

	override function unSelectAll() {
		graphListView.unSelectAll();
	}

	override def view = Stage {
		title: bind displayTitle
		width: bind appWidth with inverse
		height: bind appHeight with inverse
		visible: bind active
		scene: Scene {
			fill: Color.BLACK
			content: [
				LayoutRect {
					fill: panelBackground
					width: bind appWidth
					height: bind appHeight - verticalWindowMismatch
				},
				// the vertical box containing the (optional) upper toolbar and the rest of the GUI.
				VBox {
					spacing: mainPanelSpacing
					content: bind [
						toolBar,
						Stack {
							nodeHPos: HPos.LEFT
							nodeVPos: VPos.TOP
							content: bind [
								graphPanel,
								graphListView,
								inspectorPanel
							]
						}
					]
				}
			]
		}
	}
}


