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
import javafx.scene.*;
import javafx.scene.image.*;
import javafx.scene.paint.*;
import java.lang.System;
import java.io.File;

// Model Classes
import Carneades.Argument.Argument;
import Carneades.Argument.Argument.*;

// Other View Classes
import Carneades.Graph.*;
import Carneades.Graph.GC.*;

// Abstract Controller Class for Interaction
import Carneades.Control.GraphControl;

/**
 * The panel containing the argument graph list on the left side of the application.
 */
public class GraphListPanel extends SwingPanel {

	/**
	 * The sequence of model argument graphs to list.
	 */
	public var argumentGraphs: ArgumentGraph[] = [];

	/**
	 * The application's control object.
	 */
	public var control: GraphControl;

	/**
	 * The actual list component.
	 */
	public var list: GraphList = GraphList {
		x: 5
		y: 30
		width: bind this.width - 10
		height: bind this.height - 30
		argumentGraphs: bind argumentGraphs
		control: bind control
		visible: true
		items: bind for (g in argumentGraphs) { ArgumentGraphItem { argumentGraph: g } }
	}

	override var content = bind [
		Label {
			x: 5
			y: 5
			width: 80
			height: 20
			text: "graphs"
			visible: true
			horizontalAlignment: HorizontalAlignment.LEFT
		},
		SwingButton {
			x: this.width - 53
			y: 2
			width: 55
			height: 25
			text: "+"
			//icon: Icon { image: bind Image { url: "{__DIR__}images/icon-plus.png", size: 8 } }
			action: function() { control.addArgumentGraph(); }
		},
		list
	]
}

/**
 * Swing list subclass to display the list of available argument graphs.
 */
public class GraphList extends List {
	/**
	 * The sequence of argument graphs to be listed.
	 */
	public var argumentGraphs: ArgumentGraph[] = [];

	/**
	 * The application's control object.
	 */
	public var control: GraphControl;

	override var selectedItem = null on replace { 
		if (selectedItem != null) {
			control.switchToGraph((selectedItem as ArgumentGraphItem).argumentGraph); 
			control.unSelectGraph();
			control.unSelectList();
			insert (selectedItem as ArgumentGraphItem).argumentGraph into control.graph.selectedModels;
			control.processSelection();
		}
	};
}

/**
 * Class for the items of the graph list.
 */
class ArgumentGraphItem extends ListItem {
	
	/**
	 * The model argument graph represented by the item.
	 */
	public var argumentGraph: ArgumentGraph;

	override var text = bind { if (argumentGraph.title != "") argumentGraph.title else argumentGraph.id };
}
