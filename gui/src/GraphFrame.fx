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
import javafx.scene.paint.*;
import javafx.scene.geometry.*;
import javafx.scene.effect.*;
import javafx.scene.text.Font;
import javafx.scene.text.*;
import javafx.scene.text.FontStyle;
import javafx.scene.image.*;
import java.lang.System;
import javax.swing.JFileChooser;
import javax.swing.JDialog;
import javax.swing.JOptionPane;
import java.io.File;
import java.net.URL;
import java.util.Scanner;

// Model Classes
import Carneades.Argument.Argument;
import Carneades.Argument.Argument.*;

// Other View Classes
import Carneades.Graph.*;
import Carneades.Graph.GC.*;
import Carneades.Graph.GraphList.*;
import Carneades.Graph.Elements.Elements.*;

// Abstract Controller Class for Interaction
import Carneades.Control.GraphControl;

/**
 * The Carneades application frame and top-level gui component.
 */

public class GraphFrame extends SwingFrame {

	/**
	 * The version to be displayed in the about-box. Currently being unsuccessfully extracted from the ant environmen.t
	 */
	 var version: String = "";
	
	/**
	 * The view graph object currently displayed in the canvas.
	 */
	public var graph: Graph;

	/**
	 * The control object.
	 */
	public var control: GraphControl;

	/**
	 * The model argument graph array.
	 */
	public var argumentGraphs: ArgumentGraph[];

	/**
	 * The model argument graph whose graphical representation is currently displayed in the canvas.
	 */
	public var argumentGraph: ArgumentGraph;

	 var showCredits: Boolean = false;

	 var chooser: JFileChooser = new JFileChooser();

	override var title = "Carneades";
	override var width = appWidth on replace { if (width < appWidth) width = appWidth; };
	override var height = appHeight on replace { if (height < appHeight) height = appHeight; };
	override var background = panelBackground;

	/**
	 * The view component as the top scenegraph node which is displayed on the viewCanvas.
	 */
	public var view: GraphView = bind GraphView {
							width: bind viewCanvas.width
							height: bind viewCanvas.height
							graph: bind graph
							visible: true
							control: bind control
					}
	
	/**
	 * The canvas on the lower left of the frame in which the graph is shown.
	 */
	public var viewCanvas: Canvas = Canvas {
		preferredSize: bind [this.width-editWidth - graphListWidth - 5, this.height - toolBarHeight]
		content: bind view
	}

	/**
	 * The inspector panel to the lower right of the frame which allows for the modification of the attributes of an argument element.
	 */
	public var edit: GraphEdit = GraphEdit {
		background: panelBackground
		visible: true
		control: bind control
		argumentGraph: bind argumentGraph
		preferredSize: bind [editWidth, (this.height / 2) + 60]
	}

	/**
	 * The list of statements on the upper right of the frame.
	 */
	public var list: ElementList = ElementList {
		background: panelBackground
		visible: true
		control: bind control
		argumentGraph: bind argumentGraph
		preferredSize: bind [editWidth, (this.height /3) - 40]
	}

	override var closeAction = function(): Void {
		quit();
	}

	 var rightPanel: BorderPanel = bind BorderPanel {
		background: panelBackground
		preferredSize: bind [ editWidth, this.height ]
		top: bind list
		bottom: bind edit
		visible: true
	}

	/**
	 * The graph list panel at the left edge of the frame.
	 */
	public var graphList: GraphListPanel = GraphListPanel {
		argumentGraphs: bind argumentGraphs
		control: bind control
		preferredSize: [graphListWidth, this.height - toolBarHeight]
	}

	override var content = bind BorderPanel {
		top: bind toolPanel
		left: bind graphList
		center: bind viewCanvas
		right: bind rightPanel
	}
	
	 var toolPanel: FlowPanel = FlowPanel {
		alignment: HorizontalAlignment.LEFT
		preferredSize: [ this.width, toolBarHeight ]
		background: toolPanelBackground
		hgap: 0
		vgap: 0
		visible: true
		content: [
			SwingButton {
				width: toolBarButtonWidth
				height: toolBarButtonHeight
				icon: Icon { image: bind Image { url: "{__DIR__}images/icon-new.png" } }
				action: function() { newDocument(); }
			},
			SwingButton {
				width: toolBarButtonWidth
				height: toolBarButtonHeight
				icon: Icon { image: bind Image { url: "{__DIR__}images/icon-open.png" } }
				action: function() { open(); }
			},
			SwingButton {
				enabled: bind control.fileChanged
				width: toolBarButtonWidth
				height: toolBarButtonHeight
				icon: Icon { image: bind Image { url: "{__DIR__}images/icon-save.png" } }
				action: function() { save(); }
			},
			SpacerPanel {},
			SpacerPanel {},
			SwingButton {
				enabled: bind control.dragView
				width: toolBarButtonWidth
				height: toolBarButtonHeight
				icon: Icon { image: bind Image { url: "{__DIR__}images/icon-pointer.png" } }
				action: function() { control.dragView = false; }
			},
			SwingButton {
				enabled: bind not control.dragView
				width: toolBarButtonWidth
				height: toolBarButtonHeight
				icon: Icon { image: bind Image { url: "{__DIR__}images/icon-dragview.png" } }
				action: function() { control.dragView = true; }
			},
			SpacerPanel {},
			SpacerPanel {},
			GridPanel {
				background: toolPanelBackground
				height: toolBarHeight
				width: 60
				rows: 2
				columns: 1
				hgap: 2
				content: [
						  Label { text: "Zoom:"
								  foreground: Color.WHITE
								  horizontalAlignment: HorizontalAlignment.CENTER 
						  },
						  Label { text: bind {"{(view.zoomFactor * 100) as Integer}%"},
								  foreground: Color.WHITE 
								  horizontalAlignment: HorizontalAlignment.CENTER 
						  },
				]
			},
			GridPanel {
				background: toolPanelBackground
				height: toolBarHeight
				width: 60
				rows: 2
				columns: 1
				hgap: 0
				content: [
						  SwingButton {
							  width: toolBarHeight / 2
							  height: toolBarHeight / 2
							  text: "+"
							  //icon: Icon { image: bind Image { url: "{__DIR__}images/icon-plus.png", size: 8 } }
							  action: function() { view.zoom(-1.0); }
						  },
						  SwingButton {
							  width: toolBarHeight / 2
							  height: toolBarHeight / 2
							  text: "-"
							  //icon: Icon { image: bind Image { url: "{__DIR__}images/icon-minus.png", size: 8 } }
							  action: function() { view.zoom(1.0); }
						  },
				]
			},
			SpacerPanel {},
			SpacerPanel {},
			SwingButton {
				enabled: bind control.possibleToUndo
				width: toolBarButtonWidth
				height: toolBarButtonHeight
				icon: Icon { image: bind Image { url: "{__DIR__}images/icon-undo.png" } }
				action: function() { control.undo(); }
			},
			SwingButton {
				enabled: bind control.possibleToRedo
				width: toolBarButtonWidth
				height: toolBarButtonHeight
				icon: Icon { image: bind Image { url: "{__DIR__}images/icon-redo.png" } }
				action: function() { control.redo(); }
			},
			SpacerPanel {},
			SpacerPanel {},
			SwingButton {
				width: toolBarButtonWidth
				height: toolBarButtonHeight
				icon: Icon { image: bind Image { url: "{__DIR__}images/icon-quit.png" } }
				action: function() { quit(); }
			},
		]
	}

	// Menus
	override var menus = [
				Menu {
					text: "File"
					items: [
						MenuItem {
							text: "About Carneades"
								action: function() {
								this.showCredits = true;
							}
						},
						MenuItem {
							enabled: true;
							text: "New"
							action: function() {
								newDocument();
							} // action
						}
						, MenuItem {
							text: "Open"
							action: function() {
								open();
							}
						}
						, MenuItem {
							enabled: bind control.fileChanged
							text: "Save"
							action: function() {
								save();
							}
						}
						, MenuItem {
							text: "Save as"
							action: function() {
								saveAs();	
							}
						}
						, MenuItem {
							text: "Quit Carneades"
							action: function() {
								quit();
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
							text: "Remove"
							enabled: bind control.possibleToRemove;
							action: function() {
								control.removeSelected();
							}
						}
					] // items
				} // menu
				, Menu {
					text: "Insert"
					items: [
						MenuItem {
							text: "Argument Graph"
							action: function() {
								control.addArgumentGraph();
							}
						} // menuitem
						, MenuItem {
							text: "Statement"
							enabled: bind control.possibleToAddConclusion;
							action: function() {
								control.addStatement();
							}
						} // menuitem
						, MenuItem {
							text: "Premise"
							enabled: bind control.possibleToAddPremise;
							action: function() {
								control.addPremiseToSelected();
							}
						} // menuitem
						, MenuItem {
							text: "Argument"
							enabled: bind control.possibleToAddArgument;
							action: function() {
								control.addArgumentToSelected();
							}
						} // menuitem
					]
				} // menu
				, Menu {
					text: "debug"
					visible: bind debug
					items: [
						MenuItem {
							text: "Print selection"
							action: function() {
								control.printSelected();
							}
						},
						MenuItem {
							text: "unselect all"
							action: function() {
								control.unSelectAll();
							}
						},
						MenuItem {
							text: "print sizes"
							action: function() {
								control.printSizes();
							}
						},
						MenuItem {
							text: "print vertices"
							action: function() {
								System.out.println("# of vertices: {sizeof graph.vertices}");
								graph.print();
							}
						}

					]
				}
	]; // override default

	postinit {
		loadVersionNumber();
	}

	 var creditsFrame: SwingFrame = SwingFrame {
		visible: bind this.showCredits
		title: "About Carneades"
		height: 300
		width: 620
		resizable: false
		content: bind Canvas {
			visible: true
			background: Color.WHITE
			width: creditsFrame.width
			height: creditsFrame.height
			content: [
				Rectangle {
					x: 10
					y: 10
					width: creditsFrame.width - 20
					height: creditsFrame.height - 45
					fill: Color.WHITE
					stroke: Color.BLACK
					strokeWidth: 1
					effect: { 
						if (drawShadows) {
							DropShadow {
								color: bind shadowColor
								offsetX: bind xShadowShift
								offsetY: bind yShadowShift
								radius: bind shadowBlurRadius
							}
						} else null
					}
				},
				ImageView {
					x: 20
					y: 20
					width: bind creditsFrame.width
					height: bind creditsFrame.height
					image: Image {
						url: "{__DIR__}images/carneades.jpg"
						size: creditsFrame.height - 65
					},
				},
				Text {
					x: 200
					y: 40
					font: Font {
						size: 16
					}
					content: "Carneades"
				},
				Text { x: 200, y: 70, content: bind "Build {version}" },
				Text { x: 200, y: 100, content: "License: GPL v3" },
				Text { x: 200, y: 130, content: "Copyright © 2008" },
				Text { x: 200, y: 150, content: "Thomas F. Gordon" },
				Text { x: 200, y: 165, content: "Fraunhofer Institute for Open Communication Systems (FOKUS), Berlin" },
				Text { x: 200, y: 185, content: "Matthias Grabmair" },
				Text { x: 200, y: 200, content: "Intelligent Systems Program, University of Pittsburgh" },
				Text { x: 200, y: 230, content: "http://carneades.berlios.de" },
			]
		}

		closeAction: function() {
			showCredits = false;
		}
	}

	/**
	 * Display a string in an alert dialog box.
	 */
	public function alert(message: String): Void { 
		JOptionPane.showMessageDialog(null,
    		message,
    		"Error!",
    		JOptionPane.ERROR_MESSAGE
		);
	}

	 function quit(): Void {
		if (control.fileChanged) {
			var choice = JOptionPane.showOptionDialog(
				null, "All changes to the graph will be lost.\nSave it now?" , "Save Changes?", 
				JOptionPane.YES_NO_CANCEL_OPTION, 
				JOptionPane.QUESTION_MESSAGE, null, 
				["Save", "Don't Save", "Cancel"], null
			);
			if (choice == JOptionPane.YES_OPTION) {
				saveAs();	
				} else if (choice == JOptionPane.NO_OPTION) {
					System.exit(0);
				}
			} else {
			System.exit(0);
		}
	}

	 function newDocument(): Void {
		if (control.fileChanged) {
			var choice = JOptionPane.showOptionDialog(
					  null, "All changes to the graph will be lost.\nSave it now?" , "Save Changes?", 
					  JOptionPane.YES_NO_CANCEL_OPTION, 
					  JOptionPane.QUESTION_MESSAGE, null, 
					  ["Save", "Don't Save", "Cancel"], null
				  );
			if (choice == JOptionPane.YES_OPTION) {
				saveAs();	
			} else if (choice == JOptionPane.NO_OPTION) {
				control.newGraph();
			}
		} else {
			control.newGraph();
		}
	}

	 function open(): Void {
		if (control.fileChanged) {
			var choice = JOptionPane.showOptionDialog(
								  null, "All changes to the graph will be lost.\nSave it now?" , "Save Changes?", 
								  JOptionPane.YES_NO_CANCEL_OPTION, 
								  JOptionPane.QUESTION_MESSAGE, null, 
								  ["Save", "Don't Save", "Cancel"], null
							  );
			if (choice == JOptionPane.YES_OPTION) {
				saveAs();	
			} else if (choice == JOptionPane.NO_OPTION) {
				var returnval = chooser.showOpenDialog(null);
				if (returnval == JFileChooser.APPROVE_OPTION) {
					control.loadGraphFromFile(chooser.getSelectedFile());
				}
			}
		} else {
			var returnval = chooser.showOpenDialog(null);
			if (returnval == JFileChooser.APPROVE_OPTION) {
				control.loadGraphFromFile(chooser.getSelectedFile());
			}
		}
	}

	 function save(): Void {
		if (control.fileLoaded) {
			control.saveGraphToFile(control.currentFile);
		} else {
			// do the same as save-as
			saveAs();
		} // if loaded
	} // function

	 function saveAs(): Void {
		var returnval = chooser.showSaveDialog(null);
		if (returnval == JFileChooser.APPROVE_OPTION) {
			var file: File = chooser.getSelectedFile();
				if (file.exists()) {
					var overwrite = JOptionPane.showOptionDialog(
						null, "The file already exists.\nDo you want to overwrite it?" , "Overwrite existing file?", 
						JOptionPane.YES_NO_OPTION, 
						JOptionPane.QUESTION_MESSAGE, null,
						["Yes", "No"], null
					);
				if (overwrite == JOptionPane.OK_OPTION) { control.saveAsGraphToFile(file); }
			} else { control.saveAsGraphToFile(file); }
		}		
	}

	 function loadVersionNumber(): Void {
		var file: File = new File ("version.txt");
		if (file.canRead()) {
			var s: Scanner = new Scanner(file);
			this.version = s.next();
			s.close();
		} else {
			System.out.println("Version file not found.");
		}
	}

}

class ToolBarButton extends SwingButton {
	override var preferredSize = [toolBarHeight, toolBarHeight];
}

class SpacerPanel extends SwingPanel {
	override var visible = true;
	override var width = 10;
	override var height = 10;
	override var background = Color.rgb(0,0,0,0);
}
