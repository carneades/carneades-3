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
import carneadesgui.control.CarneadesControl;

import javafx.scene.control.ToggleButton;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;
import javafx.scene.control.ToggleGroup;
import javafx.scene.Group;
import javafx.scene.control.ListView;
import javafx.scene.control.TextBox;
import javafx.scene.input.MouseEvent;

import java.lang.String;

// LayoutInfo objects needed
var graphListLayoutInfo: LayoutInfo = LayoutInfo {
		minWidth: bind inspectorPanelWidth;
		width: bind inspectorPanelWidth;
		minHeight: bind listViewHeight;
		height: bind listViewHeight;
}

var listLayoutInfo: LayoutInfo = LayoutInfo {
		minWidth: bind inspectorPanelWidth;
		width: bind inspectorPanelWidth;
		minHeight: bind listViewHeight - tabButtonHeight;
		height: bind listViewHeight - tabButtonHeight;
}

var listEntryLayoutInfo: LayoutInfo = LayoutInfo {
		minWidth: bind inspectorPanelWidth;
		width: bind inspectorPanelWidth;
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
	//override var layoutInfo = bind listLayoutInfo;
	override var width = inspectorPanelWidth;
	override var height = listViewHeight - tabButtonHeight - listEntryFieldHeight;

	override var items = bind listItems;
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
			if ((filter == "") or matches(filter, s.toLowerCase())) {
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
	override var layoutInfo = graphListLayoutInfo;
	public var control: CarneadesControl = null;
	public var view: CarneadesView;
	var mode: Integer = listStatementMode;

	var tabGroup: ToggleGroup = ToggleGroup {}

	var graphTabButton: ToggleButton = TabButton {
		layoutInfo: tabButtonLayoutInfo
		selected: {mode == listGraphMode}
		toggleGroup: tabGroup
		text: "graphs"
		onMouseClicked: function(e: MouseEvent) { mode = listGraphMode; }
	}

	var statementTabButton: ToggleButton = TabButton {
		layoutInfo: tabButtonLayoutInfo
		toggleGroup: tabGroup
		selected: {mode == listStatementMode}
		text: "statements"
		onMouseClicked: function(e: MouseEvent) { mode = listStatementMode; }
	}

	var argumentTabButton: ToggleButton = TabButton {
		layoutInfo: tabButtonLayoutInfo
		toggleGroup: tabGroup
		selected: {mode == listArgumentMode}
		text: "arguments"
		onMouseClicked: function(e: MouseEvent) { mode = listArgumentMode; }
	}

	var listGroup: GraphListGroup = GraphListGroup {};

	var graphList: GraphList = GraphList {
		layoutInfo: listLayoutInfo
		visible: bind {mode == listGraphMode}
	}
	var graphListEntryField: ListEntryField = ListEntryField {
		listView: graphList
	}

	var statementList: GraphList = GraphList {
		group: listGroup
		layoutInfo: listLayoutInfo
		visible: bind {mode == listStatementMode}
	}
	var statementListEntryField: ListEntryField = ListEntryField {
		listView: statementList
	}

	var argumentList: GraphList = GraphList {
		group: listGroup
		layoutInfo: listLayoutInfo
		visible: bind {mode == listArgumentMode}
	}
	var argumentListEntryField: ListEntryField = ListEntryField {
		listView: argumentList
	}

	override var content = [
		LayoutRect {
			layoutInfo: graphListLayoutInfo
			fill: panelBackground
		},
		VBox {
			layoutInfo: graphListLayoutInfo
			content: bind [
				HBox {
					content: bind [graphTabButton, statementTabButton, argumentTabButton]
				},
				{
					if (mode == listGraphMode) graphListEntryField
					else if (mode == listStatementMode) statementListEntryField
					else /*(mode == listArgumentMode)*/ argumentListEntryField
				},
				Group {
					content: bind [	graphList, statementList, argumentList ]
				}
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

