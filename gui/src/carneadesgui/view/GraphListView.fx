/*
 * GraphList.fx
 *
 * Created on 08.07.2009, 15:44:55
 */

package carneadesgui.view;

import javafx.scene.layout.LayoutInfo;

import carneadesgui.GC.*;
import carneadesgui.model.Argument;
import carneadesgui.model.Argument.*;
import carneadesgui.view.GraphUpdate;
import carneadesgui.control.CarneadesControl;

import javafx.scene.control.ToggleButton;
import javafx.scene.layout.HBox;
import javafx.scene.control.ToggleGroup;
import javafx.scene.control.ListView;
import javafx.scene.control.TextBox;
import javafx.scene.input.MouseEvent;
import java.lang.String;
import javafx.scene.layout.VBox;

import javafx.scene.Node;


import javafx.scene.control.Button;

def listEntryLayoutInfo: LayoutInfo = LayoutInfo {
	minWidth: bind inspectorPanelWidth - 2*INSPECTOR_PADDING;
	width: bind inspectorPanelWidth - 2*INSPECTOR_PADDING;
	minHeight: bind listEntryFieldHeight;
	height: bind listEntryFieldHeight;
}

def tabButtonLayoutInfo: LayoutInfo = LayoutInfo {
	maxHeight: bind tabButtonHeight;
	minHeight: bind tabButtonHeight;
	height: bind tabButtonHeight;
}

// improved widget classes
class TabButton extends Button {
	public var toggleGroup: ToggleGroup;
	override def layoutInfo = tabButtonLayoutInfo;
}

class ListEntryField extends TextBox {
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
public class GraphListView extends MoveablePanel {
	override def title = "Search Elements";
	override def width = inspectorPanelWidth;
	override def height = GRAPHLISTVIEW_HEIGHT;
	override def padding = INSPECTOR_PADDING;

	public var control: CarneadesControl = null;
	public var view: CarneadesView;

	override def onClose = function() {
		hide();
	}

	var mode: Integer = listStatementMode;

	def graphTabButton: TabButton = TabButton {
		strong: bind {mode == listGraphMode}
		text: "graphs"
		onMouseClicked: function(e: MouseEvent) {
			mode = listGraphMode;
		}
	}

	def statementTabButton: TabButton = TabButton {
		strong: bind {mode == listStatementMode}
		text: "statements"
		onMouseClicked: function(e: MouseEvent) {
			mode = listStatementMode;
		}
	}

	def argumentTabButton: TabButton = TabButton {
		strong: bind {mode == listArgumentMode}
		text: "arguments"
		onMouseClicked: function(e: MouseEvent) {
			mode = listArgumentMode;
		}
	}

	def listGroup: GraphListGroup = GraphListGroup {};

	def graphListLayoutInfo: LayoutInfo = LayoutInfo{
		def w: Number = inspectorPanelWidth - 2 * INSPECTOR_PADDING
		def h: Number = GRAPHLISTVIEW_HEIGHT - 3 * INSPECTOR_PADDING - tabButtonHeight - 2 * INSPECTOR_PANEL_SPACING - MOVEABLEPANEL_TITLE_HEIGHT - listEntryFieldHeight
		height: h
		maxHeight: h
		width: w
		maxWidth: w
	}

	def entryFieldLayoutInfo: LayoutInfo = LayoutInfo{
		def w: Number = inspectorPanelWidth - 2 * INSPECTOR_PADDING
		def h: Number = listEntryFieldHeight
		height: h
		maxHeight: h
		width: w
		maxWidth: w
	}

	def graphList: GraphList = GraphList {
		layoutInfo: graphListLayoutInfo
	}
	
	def graphListEntryField: ListEntryField = ListEntryField {
		listView: graphList
		layoutInfo: entryFieldLayoutInfo
	}

	def statementList: GraphList = GraphList {
		group: listGroup
		layoutInfo: graphListLayoutInfo
	}

	def statementListEntryField: ListEntryField = ListEntryField {
		listView: statementList
		layoutInfo: entryFieldLayoutInfo
	}

	def argumentList: GraphList = GraphList {
		group: listGroup
		layoutInfo: graphListLayoutInfo
	}
	def argumentListEntryField: ListEntryField = ListEntryField {
		listView: argumentList
		layoutInfo: entryFieldLayoutInfo
	}

	def listComponents: Node[] =
		bind 
			if (mode == listGraphMode)
				[graphListEntryField, graphList]
			else if (mode == listStatementMode)
				[ statementListEntryField, statementList ]
			else /*(mode == listArgumentMode)*/
				[ argumentListEntryField, argumentList];

	override var content = bind VBox {
		spacing: GRAPHLISTVIEW_SPACING
		content: bind [
			HBox { content: bind [  graphTabButton, statementTabButton, argumentTabButton ] },
			listComponents
		]
	}

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

