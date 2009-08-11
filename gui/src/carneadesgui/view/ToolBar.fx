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


class ToolBarButton extends ImageButton {
	override var width = toolBarHeight - 10;
	override var height = toolBarHeight - 10;
	public var control: CarneadesControl;
}

/**
* The class for the upper toolbar of the standard view.
*/
public class ToolBar extends Panel {
	public var control: CarneadesControl;

	var debugButton: Button = Button {
		text: "debug"
		onMouseClicked: function(e: MouseEvent): Void {}
	}

	var openButton: ToolBarButton = ToolBarButton {
		//text: "open"
		control: bind control
		image: Image { url: "{__DIR__}images/icon-open.png"	}
		action: function(): Void { control.open(); }
	};

	var saveButton: ToolBarButton = ToolBarButton {
		//text: "save"
		disable: bind not control.fileChanged
		control: bind control
		image: Image { url: "{__DIR__}images/icon-save.png"	}
		action: function(): Void { control.save(); }
	};

	var saveAsButton: ToolBarButton = ToolBarButton {
		//text: "save as"
		control: bind control
		image: Image { url: "{__DIR__}images/icon-saveas.png"	}
		action: function(): Void { control.saveAs(); }
	};

	var undoButton: ToolBarButton = ToolBarButton {
		//text: "undo"
		disable: bind not control.possibleToUndo
		control: bind control
		image: Image { url: "{__DIR__}images/icon-undo.png"	}
		action: function(): Void { control.undo(); }
	};

	var redoButton: ToolBarButton = ToolBarButton {
		// text: "redo"
		disable: bind not control.possibleToRedo
		control: bind control
		image: Image { url: "{__DIR__}images/icon-redo.png"	}
		action: function(): Void { control.redo(); }
	};

	var saveAsImageButton: ToolBarButton = ToolBarButton {
		control: bind control
		image: Image { url: "{__DIR__}images/icon-print.png"	}
		action: function(): Void { control.saveGraphAsImage(); }
	};

	var quitButton: ToolBarButton = ToolBarButton {
		// text: "quit"
		control: bind control
		image: Image { url: "{__DIR__}images/icon-quit.png"	}
		action: function(): Void { control.quit(); }
	};

	var alternateViewButton: ToolBarButton = ToolBarButton {
		// text: "alternate\nview"
		control: bind control
		action: function(): Void { control.alternateView(); }
	};

	override var content = bind [
		LayoutRect {
			width: bind appWidth
			height: bind toolBarHeight
			fill: bind toolPanelBackground
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
				//debugButton,
				openButton,
				saveButton,
				saveAsButton,
				undoButton,
				redoButton,
				//saveAsImageButton,
				quitButton,
				// alternateViewButton
			]
		}
	];
}

