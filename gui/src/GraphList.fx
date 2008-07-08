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


package GraphSketch1.Graph;

import javafx.ext.swing.*;
import javafx.scene.*;
import javafx.scene.paint.*;
import java.lang.System;
import java.io.File;

// Model Classes
import GraphSketch1.Argument.Argument;
import GraphSketch1.Argument.Argument.*;

// Other View Classes
import GraphSketch1.Graph.*;
import GraphSketch1.Graph.Elements.Elements.*;

// Abstract Controller Class for Interaction
import GraphSketch1.Control.GraphControl;


public class GraphList extends FlowPanel {

	override attribute background = GC.panelBackground;
	override attribute alignment = HorizontalAlignment.LEFT;

	attribute control: GraphControl;
	attribute argumentGraph: ArgumentGraph;

	attribute input: TextField = TextField {
		preferredSize: bind [GC.editWidth-65, 20]
		visible: true
	}

	public attribute list: StatementList = StatementList {
		control: bind control
		preferredSize: bind [this.width-10, this.height - 60]
		visible: true
		statements: bind argumentGraph.statements
		filter: bind input.text.toLowerCase()
	}

	attribute addArgumentButton: Button = Button {
		text: "add argument"
		enabled: bind list.selectedItem != null
		action: function(): Void {
			control.addArgumentToSelected();
		}
	}

	override attribute content = bind [ Label {text: "search "}, input, 
										list,
										addArgumentButton]

}

public class StatementList extends List {
	public attribute control: GraphControl;
	attribute filter: String = "";

	override attribute selectedItem = null on replace {
		if (selectedItem != null) {
			control.unSelectGraph();
			control.processSelection();
		}
	};

	attribute statements: Statement[];

	override attribute items = bind [ 
									for (s in statements where (matches(filter, s.id.toLowerCase()) or matches(filter, s.wff.toLowerCase()))) {
									 	StatementItem {
											statement: s
											text: bind "{s.wff}"
										}
									} 
								];

	private function matches(filter: String, text: String): Boolean {
		var result: Boolean = false;
		for (i in [0 .. (text.length()-1)]) {
			if (text.substring(i).startsWith(filter)) {
				result = true;
			}
		}
		return result;
	}
}

public class StatementItem extends ListItem {
	public attribute statement: Statement;
	attribute visible: Boolean = true;
}
