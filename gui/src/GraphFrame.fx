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
import javafx.scene.paint.*;
import javafx.scene.image.*;
import java.lang.System;
import javax.swing.JFileChooser;
import java.io.File;

// Model Classes
import Carneades.Argument.Argument;
import Carneades.Argument.Argument.*;

// Other View Classes
import Carneades.Graph.*;
import Carneades.Graph.Elements.Elements.*;

// Abstract Controller Class for Interaction
import Carneades.Control.GraphControl;

public class GraphFrame extends Frame {
	
	public attribute graph: Graph;
	public attribute layout: GraphLayout;
	public attribute control: GraphControl;
	public attribute argumentGraph: ArgumentGraph;

	private attribute showCredits: Boolean = false;
	private attribute showAlert: Boolean = false;
	private attribute alertText: String = "";

	attribute chooser: JFileChooser = new JFileChooser();

	override attribute title = "Carneades";
	override attribute width = GC.appWidth;
	override attribute height = GC.appHeight;
	override attribute background = Color.WHITE;

	public attribute view: GraphView = bind GraphView {
							x: 0
							y: 0
							//preferredSize: bind [this.width-GC.editWidth-20, 100]
							graph: bind graph
							layout: bind layout
							visible: true
							control: bind control
							//preferredSize: [scroll.width, scroll.height]
					}

	public attribute edit: GraphEdit = GraphEdit {
		visible: true
		control: bind control
		argumentGraph: bind argumentGraph
		preferredSize: bind [GC.editWidth, (this.height / 2)]
	}

	public attribute list: GraphList = GraphList {
		visible: true
		control: bind control
		argumentGraph: bind argumentGraph
		preferredSize: bind [GC.editWidth, (this.height /3)]
	}


	attribute scroll: ScrollPane = ScrollPane {
		view: bind view
		preferredSize: bind [this.width - GC.editWidth-10, this.height]
	}

	override attribute closeAction = function(): Void {
		System.exit(0);
	}

	// content
	attribute rightPanel: BorderPanel = bind BorderPanel {
		//background: GC.panelBackground
		preferredSize: bind [ GC.editWidth, this.height ]
		top: bind list
		bottom: bind edit
		visible: true
	}

	// Alert Box
	private attribute alertFrame: Frame = Frame {
		title: "Alert!"
		height: 100
		visible: bind this.showAlert
		content: BorderPanel {
			top: Label { text: bind alertText }
			bottom: Button {
				text: "Ok"
				action: function() {
					this.showAlert = false;
				}
			}
		}
	}

	private attribute creditsFrame: Frame = Frame {
		title: "About Carneades"
		height: 120
		visible: bind this.showCredits
		content: BorderPanel {
			top: GridPanel {
				columns: 1
				rows: 4
				content: [
					Label {
						text: "Carneades GUI Alpha"
					},
					Label {
						text: "Version 0.0.6"
					},
					Label {
						text: "Copyright 2008, T. Gordon & M. Grabmair"
					},
					Label {
						text: "http://carneades.berlios.de"
					}
				]
			}
			bottom: Button {
				text: "Ok"
				action: function() {
					this.showCredits = false;
				}
			}
		}
	}

	public function alert(message: String) { 
		this.alertText = message;
		this.showAlert = true; 
	}

	private attribute toolPanel: Panel = FlowPanel {
		preferredSize: [ this.width, GC.toolBarHeight ]
		background: GC.toolPanelBackground
		visible: true
		/*
		content: [
			ToolBarButton {
				icon: Icon {
					image: Image {
						height: 40
						width: 40
					}
				}
			}
		]
		*/
	}

	override attribute content = bind BorderPanel {
		top: bind toolPanel
		left: bind scroll
		right: bind rightPanel
		center: null
	}
	
	// Menus
	override attribute menus = [
				Menu {
					text: "File"
					items: [
						MenuItem {
							enabled: true;
							text: "New"
							action: function() {
								control.newGraph();
							}
						}
						, MenuItem {
							text: "Open"
							action: function() {
								var returnval = chooser.showOpenDialog(null);
								if (returnval == JFileChooser.APPROVE_OPTION) {
									control.loadGraphFromFile(chooser.getSelectedFile());
								}
							}
						}
						, MenuItem {
							enabled: bind control.fileChanged
							text: "Save"
							action: function() {
								if (control.fileLoaded) {
									control.saveGraphToFile(control.currentFile);
								} else {
									var returnval = chooser.showSaveDialog(null);
									if (returnval == JFileChooser.APPROVE_OPTION) {
										control.saveAsGraphToFile(chooser.getSelectedFile());
									}
								}
							}
						}
						, MenuItem {
							text: "Save as"
							action: function() {
								var returnval = chooser.showSaveDialog(null);
								if (returnval == JFileChooser.APPROVE_OPTION) {
									control.saveAsGraphToFile(chooser.getSelectedFile());
								}
							}
						}
						/*,
						MenuItem {
							enabled: true;
							text: "Update"
							action: function() {
								control.updateAll();
							}
						}*/
						, MenuItem {
							text: "Quit Carneades"
							action: function() {
								System.exit(0);
							}
						}
						] // content
					} // menu
				, Menu {
					text: "Edit"
					items: [
						MenuItem {
							text: "Undo"
							enabled: bind control.possibleToUndo;
							action: function() {
								control.undo();
							}
						} // menuitem
						, MenuItem {
							text: "Redo"
							enabled: bind control.possibleToRedo;
							action: function() {
								control.redo();
							}
						} // menuitem
						, MenuItem {
							text: "New Statement"
							enabled: bind control.possibleToAddConclusion;
							action: function() {
								control.addStatement();
							}
						} // menuitem
						, MenuItem {
							text: "Add Premise"
							enabled: bind control.possibleToAddPremise;
							action: function() {
								control.addPremiseToSelected();
							}
						} // menuitem
						, MenuItem {
							text: "Add Argument"
							enabled: bind control.possibleToAddArgument;
							action: function() {
								control.addArgumentToSelected();
							}
						} // menuitem
						, MenuItem {
							text: "Delete"
							enabled: bind control.possibleToDelete;
							action: function() {
								control.deleteSelected();
							}
						}
					] // items
				} // menu
				, Menu {
					text: "Help"
					items: [
					MenuItem {
						text: "about Carneades"
						action: function() {
							this.showCredits = true;
						}
					}
					] // items
				} // menu
	]; // override default

	postinit {
		if (GC.releaseVersion) showCredits = true;
	}

}

class ToolBarButton extends Button {
	override attribute preferredSize = [GC.toolBarHeight, GC.toolBarHeight];
}
