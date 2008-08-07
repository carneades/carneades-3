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

// Abstract Controller Class for Interaction
import Carneades.Control.GraphControl;


public class GraphListPanel extends SwingPanel {
	public attribute argumentGraphs: ArgumentGraph[] = [];
	public attribute control: GraphControl;
	public attribute list: GraphList = GraphList {
		x: 5
		y: 30
		width: bind this.width - 10
		height: bind this.height - 30
		argumentGraphs: bind argumentGraphs
		control: bind control
		visible: true
		items: bind for (g in argumentGraphs) { ArgumentGraphItem { argumentGraph: g } }
	}

	override attribute content = bind [
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
			x: this.width - 30
			y: 2
			width: 25
			height: 25
			icon: Icon { image: bind Image { url: "{__DIR__}images/icon-plus.png", size: 8 } }
			action: function() { control.addArgumentGraph(); }
		},
		list
	]
}

public class GraphList extends List {
	public attribute argumentGraphs: ArgumentGraph[] = [];
	public attribute control: GraphControl;
	override attribute selectedItem = null on replace { 
		if (selectedItem != null) {
			control.switchToGraph((selectedItem as ArgumentGraphItem).argumentGraph); 
			control.unSelectGraph();
			control.unSelectList();
			insert (selectedItem as ArgumentGraphItem).argumentGraph into control.graph.selectedModels;
			control.processSelection();
		}
	};
}

class ArgumentGraphItem extends ListItem {
	public attribute argumentGraph: ArgumentGraph;
	override attribute text = bind { if (argumentGraph.title != "") argumentGraph.title else argumentGraph.id };
}
