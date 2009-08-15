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
import carneadesgui.GC.*;
import carneadesgui.control.CarneadesControl;
import javafx.scene.layout.HBox;
import javafx.scene.input.MouseEvent;
import javafx.scene.control.Button;
import javafx.scene.layout.LayoutInfo;
import javafx.geometry.VPos;
import javafx.scene.image.Image;
import javafx.scene.effect.Glow;
import javafx.scene.shape.Rectangle;

import javafx.scene.image.ImageView;


class ToolBarButton extends Button {
	public var image: Image;

	def imageView: ImageView = ImageView {
		smooth: true
		image: bind image
	}

	override def graphic = bind imageView;
	public var control: CarneadesControl;

	override def layoutInfo = LayoutInfo {
		minWidth: toolBarHeight - 10
		width: toolBarHeight - 10
		minHeight: toolBarHeight - 10
		height: toolBarHeight - 10
	}
}



/**
* The class for the upper toolbar of the standard view.
*/
public class ToolBar extends Panel {
	public var control: CarneadesControl;
	public var view: CarneadesView;
	public def mode: Integer = bind view.mode;

	def debugButton: Button = Button {
		text: "debug"
		onMouseClicked: function(e: MouseEvent): Void {}
	}

	def openButton: ToolBarButton = ToolBarButton {
		//text: "open"
		control: bind control
		image: Image { url: "{__DIR__}images/icon-open.png"	}
		action: function(): Void { control.open(); }
	};

	def saveButton: ToolBarButton = ToolBarButton {
		//text: "save"
		disable: bind not control.fileChanged
		control: bind control
		image: Image { url: "{__DIR__}images/icon-save.png"	}
		action: function(): Void { control.save(); }
	};

	def saveAsButton: ToolBarButton = ToolBarButton {
		//text: "save as"
		control: bind control
		image: Image { url: "{__DIR__}images/icon-saveas.png"	}
		action: function(): Void { control.saveAs(); }
	};

	def undoButton: ToolBarButton = ToolBarButton {
		//text: "undo"
		disable: bind not control.possibleToUndo
		control: bind control
		image: Image { url: "{__DIR__}images/icon-undo.png"	}
		action: function(): Void { control.undo(); }
	};

	def redoButton: ToolBarButton = ToolBarButton {
		// text: "redo"
		disable: bind not control.possibleToRedo
		control: bind control
		image: Image { url: "{__DIR__}images/icon-redo.png"	}
		action: function(): Void { control.redo(); }
	};

	def displayGraphListButton: ToolBarButton = ToolBarButton {
		control: bind control
		image: Image { url: "{__DIR__}images/icon-searchedit.png"	}
		action: function(): Void { control.displayGraphListView(); }
	};

	def saveAsImageButton: ToolBarButton = ToolBarButton {
		control: bind control
		image: Image { url: "{__DIR__}images/icon-print.png"	}
		action: function(): Void { control.saveGraphAsImage(); }
	};

	def quitButton: ToolBarButton = ToolBarButton {
		control: bind control
		image: Image { url: "{__DIR__}images/icon-quit.png"	}
		action: function(): Void { control.quit(); }
	};

	def aboutButton: ToolBarButton = ToolBarButton {
		control: bind control
		image: Image { url: "{__DIR__}images/icon-about.png"	}
		action: function(): Void { view.displayAboutInformation(); }
	};

	def alternateViewButton: ToolBarButton = ToolBarButton {
		// text: "alternate\nview"
		control: bind control
		action: function(): Void { control.alternateView(); }
	};

	def addGraphButton: ToolBarButton = ToolBarButton {
		control: bind control
		image: Image { url: "{__DIR__}images/icon-newelement.png"	}
		action: function(): Void { control.addArgumentGraph(control.defaultArgumentGraph(control.getNewGraphId())); }
	}

	def removeGraphButton: ToolBarButton = ToolBarButton {
		control: bind control
		image: Image { url: "{__DIR__}images/icon-stop.png"	}
		action: function(): Void { control.removeCurrentArgumentGraph(); }
	}

	def addStatementButton: ToolBarButton = ToolBarButton {
		control: bind control
		image: Image { url: "{__DIR__}images/icon-newbox.png"	}
		action: function(): Void { control.addStatement(); }
	}

	def removeStatementButton: ToolBarButton = ToolBarButton {
		control: bind control
		disable: bind not control.possibleToRemove
		image: Image { url: "{__DIR__}images/icon-stop.png"	}
		action: function(): Void { control.removeSelected(); }
	}

	def addArgumentButton: ToolBarButton = ToolBarButton {
		control: bind control
		disable: bind not control.possibleToAddArgument
		image: Image { url: "{__DIR__}images/icon-newelement.png"	}
		action: function(): Void { control.addArgumentToSelected(); }
	}

	def removeArgumentButton: ToolBarButton = ToolBarButton {
		control: bind control
		disable: bind not control.possibleToRemove
		image: Image { url: "{__DIR__}images/icon-stop.png"	}
		action: function(): Void { control.removeArgumentFromBox(null); }
	}

	def addPremiseButton: ToolBarButton = ToolBarButton {
		control: bind control
		disable: bind not control.possibleToAddPremise
		image: Image { url: "{__DIR__}images/icon-newelement.png" }
		action: function(): Void { control.addPremiseToSelected(); }
	}

	def deadButton1: ToolBarButton = ToolBarButton { disable: true }
	def deadButton2: ToolBarButton = ToolBarButton { disable: true }
	def deadButton3: ToolBarButton = ToolBarButton { disable: true }

	override var content = bind [
		LayoutRect {
			width: bind appWidth
			height: bind toolBarHeight
			fill: bind toolPanelBackground
			effect: Glow {
				level: 0.5
			}
		},
		HBox {
			vpos: VPos.CENTER
			spacing: toolBarSpacing
			layoutInfo: LayoutInfo {
				width: bind appWidth
				minWidth: bind appWidth
				height: bind toolBarHeight
				minHeight: bind toolBarHeight
			}
			content: bind [
				Rectangle {}, // dead filler rectangle
				//debugButton,
				openButton,
				saveButton,
				saveAsButton,
				Rectangle {}, // dead filler rectangle
				undoButton,
				redoButton,
				Rectangle {}, // dead filler rectangle
				if (mode == inspectorDefaultMode) [addGraphButton, removeGraphButton, addStatementButton]
				else if (mode == inspectorStatementMode) [addArgumentButton, removeStatementButton, deadButton1]
				else if (mode == inspectorArgumentMode) [addPremiseButton, removeArgumentButton, deadButton1]
				else if (mode == inspectorPremiseMode) [deadButton1, deadButton2, deadButton3]
				else null,
				Rectangle {}, // dead filler rectangle
				displayGraphListButton,
				Rectangle {}, // dead filler rectangle
				aboutButton,
				quitButton,
			]
		}
	];
}

