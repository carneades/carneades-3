/*
 * GraphList.fx
 *
 * Created on 08.07.2009, 15:44:55
 */

package carneadesgui.view;

import javafx.scene.layout.Panel;
import javafx.scene.layout.LayoutInfo;

import carneadesgui.GC.*;
import carneadesgui.model.Argument;
import carneadesgui.model.Argument.*;
import carneadesgui.view.GraphUpdate;
import carneadesgui.view.ImageButton;
import carneadesgui.control.CarneadesControl;

import javafx.scene.control.ToggleButton;
import javafx.scene.layout.HBox;
import javafx.scene.control.ToggleGroup;
import javafx.scene.Group;
import javafx.scene.control.ListView;
import javafx.scene.control.TextBox;
import javafx.scene.input.MouseEvent;
import java.lang.String;
import javafx.scene.paint.Color;

import javafx.geometry.HPos;

import javafx.scene.image.Image;

import javafx.scene.shape.Rectangle;

import javafx.scene.layout.VBox;

var listEntryLayoutInfo: LayoutInfo = LayoutInfo {
	minWidth: bind inspectorPanelWidth - 2*INSPECTOR_PADDING;
	width: bind inspectorPanelWidth - 2*INSPECTOR_PADDING;
	minHeight: bind listEntryFieldHeight;
	height: bind listEntryFieldHeight;
}

var tabButtonLayoutInfo: LayoutInfo = LayoutInfo {
		minHeight: bind tabButtonHeight;
		height: bind tabButtonHeight;
}


// improved widget classes
class TabButton extends ToggleButton {
	override var layoutInfo = tabButtonLayoutInfo;
}

class ListEntryField extends TextBox {
	override var layoutInfo = listEntryLayoutInfo;
	override var action = function() { listView.filter = text; };
	public var listView: GraphList = null;
}

class ListItem {
	public var model: Object = null;

	override function toString() {
		if (model instanceof Argument) { return (model as Argument).id }
		else if (model instanceof Statement) { return (model as Statement).wff }
		else /*(model instanceof argumentGraph)*/ { return (model as ArgumentGraph).title }
	}
}


/**
* Group class for objects of GraphList to only allow one selection across them.
*/
class GraphListGroup {
	var lists: GraphList[] = [];
	public function register(l: GraphList) {
		insert l into lists;
	}
	public function administer(l: GraphList) {
		for (i in lists) if (i != l) i.clearSelection();
	}
}


class GraphList extends ListView {
	override var items = bind listItems;
	//override var layoutInfo = listLayoutInfo;
	// The ListView does not adhere to its layoutInfo attribute in JavaFX 1.2
	override var width = bind inspectorPanelWidth - 2*INSPECTOR_PADDING - 2;
	override var height = 
		bind { if (not minimized) GRAPHLISTVIEW_HEIGHT else GRAPHLISTVIEW_MINIMIZED_HEIGHT }
		- tabButtonHeight  - 3*GRAPHLISTVIEW_SPACING
		- listEntryFieldHeight - 2*INSPECTOR_PADDING;
	public var listItems: ListItem[] = null;
	public var listedModels: Object[] = null;

	public var group: GraphListGroup = null;
	postinit {
		if (group != null) group.register(this);
	}

	// functionality to get an event-call in case the selection changes
	public var previousSelection: Integer = -1;
	override var onMouseClicked = function(e: MouseEvent) {
		if (selectedIndex != previousSelection) {
			previousSelection = selectedIndex;
			onSelectionChange();
		}
	}
	public function onSelectionChange(): Void {
		// make the group administer things
		group.administer(this);

		// have the controller process things
		control.processListSelection((selectedItem as ListItem).model);
	}
	override function clearSelection() {
		previousSelection = -1;
		super.clearSelection();
	};

	// helper function for the filter to match strings
	function matches(filter: String, text: String): Boolean {
		var result: Boolean = false;
		for (i in [0 .. (text.length()-1)]) {
			if (text.substring(i).startsWith(filter)) {
				result = true;
			}
		}
		return result;
	}

	// the function is called updateList() as update() is already defined and I am not sure whether I can overide it without breaking some functionality.
	public function updateList(): Void {

		// save selected model item
		var savedModel: Object = (selectedItem as ListItem).model;

		// empty the list
		listItems = [];

		// fill the list of items
		for (m in listedModels) {
			var item: ListItem = ListItem {
				model: m
			};
			var s: String = item.toString();
			if ((filter == "") or matches(filter.toLowerCase(), s.toLowerCase())) {
				insert item into listItems
			}
		}

		// restore selection
		for (i in [0..sizeof items-1]) {
			if ((items[i] as ListItem).model == savedModel) select(i);
		}
	}
	public var filter: String = "" on replace { updateList(); };
}

/**
* The Panel containing the tabs, entry field and list.
*/
public class GraphListView extends Panel {
	public-read var minimized: Boolean = false;
	override var width = inspectorPanelWidth;
	override var height = { if (not minimized) GRAPHLISTVIEW_HEIGHT else GRAPHLISTVIEW_MINIMIZED_HEIGHT };
	public var control: CarneadesControl = null;
	public var view: CarneadesView;
	var mode: Integer = listStatementMode;

	var tabGroup: ToggleGroup = ToggleGroup {}

	var graphTabButton: ToggleButton = TabButton {
		selected: {mode == listGraphMode}
		toggleGroup: tabGroup
		text: "graphs"
		onMouseClicked: function(e: MouseEvent) {
			if (minimized) minimized = false;
			mode = listGraphMode;
		}
	}

	var statementTabButton: ToggleButton = TabButton {
		toggleGroup: tabGroup
		selected: {mode == listStatementMode}
		text: "statements"
		onMouseClicked: function(e: MouseEvent) {
			if (minimized) minimized = false;
			mode = listStatementMode;
		}
	}

	var argumentTabButton: ToggleButton = TabButton {
		toggleGroup: tabGroup
		selected: {mode == listArgumentMode}
		text: "arguments"
		onMouseClicked: function(e: MouseEvent) {
			if (minimized) minimized = false;
			mode = listArgumentMode;
		}
	}

	def minimizeIcon: Image = Image { url: "{__DIR__}images/icon-minimize.png"}
	def maximizeIcon: Image = Image { url: "{__DIR__}images/icon-maximize.png"}
	def sizeButton: ImageButton = ImageButton {
		width: tabButtonHeight - 2
		height: tabButtonHeight - 2
		image: bind if (minimized) maximizeIcon else minimizeIcon
		action: function() {
			if (minimized) minimized = false else minimized = true
		}
	}

	var listGroup: GraphListGroup = GraphListGroup {};

	var graphList: GraphList = GraphList {
	}
	
	var graphListEntryField: ListEntryField = ListEntryField {
		listView: graphList
	}

	var statementList: GraphList = GraphList {
		group: listGroup
	}
	var statementListEntryField: ListEntryField = ListEntryField {
		listView: statementList
	}

	var argumentList: GraphList = GraphList {
		group: listGroup
	}
	var argumentListEntryField: ListEntryField = ListEntryField {
		listView: argumentList
	}

	def listComponents: Group = Group {
		content: bind [ 
			if (mode == listGraphMode)
				VBox {
					spacing: GRAPHLISTVIEW_SPACING
					content: bind [graphListEntryField, graphList]
				}
			else if (mode == listStatementMode)
				VBox {
					spacing: GRAPHLISTVIEW_SPACING
					content: bind [ statementListEntryField, statementList ]
				}
			else /*(mode == listArgumentMode)*/
				VBox { 
					spacing: GRAPHLISTVIEW_SPACING
					content: bind [ argumentListEntryField, argumentList]
				}
		]
	}

	override var content = [
		LayoutRect {
			width: inspectorPanelWidth
			height: bind { if (not minimized) GRAPHLISTVIEW_HEIGHT else GRAPHLISTVIEW_MINIMIZED_HEIGHT }
			fill: panelBackground
			stroke: Color.BLACK
		},
		PaddedVBox {
			//width: inspectorPanelWidth
			//height: bind { if (not minimized) GRAPHLISTVIEW_HEIGHT else GRAPHLISTVIEW_MINIMIZED_HEIGHT }
			nodeHPos: HPos.LEFT
			spacing: GRAPHLISTVIEW_SPACING
			xPadding: INSPECTOR_PADDING
			yPadding: INSPECTOR_PADDING
			content: bind [
				HBox {
					content: bind [
						graphTabButton, statementTabButton, argumentTabButton,
						Rectangle{ width: 20}, sizeButton
					]
				},
				if (not minimized) listComponents else null
			]
		}
	];

	// unselect stuff
	public function unSelectAll(): Void {
		graphList.clearSelection();
		statementList.clearSelection();
		argumentList.clearSelection();
	}

	/**
	* Update the lists and reset their selection.
	*/
	public function update(u: GraphUpdate): Void {
		graphList.listedModels = control.argumentGraphs;
		statementList.listedModels = control.argumentGraph.statements;
		argumentList.listedModels = control.argumentGraph.arguments;

		graphList.updateList();
		statementList.updateList();
		argumentList.updateList();
	}
}

