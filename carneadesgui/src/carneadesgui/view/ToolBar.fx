/*
 * ToolBar.fx
 *
 * Created on 14.07.2009, 14:39:47
 */

package carneadesgui.view;

import javafx.scene.layout.Panel;
import carneadesgui.GC.*;
import carneadesgui.control.CarneadesControl;
import javafx.scene.image.ImageView;
import javafx.scene.layout.HBox;
import javafx.scene.input.MouseEvent;

import javafx.scene.control.Button;

import javafx.scene.layout.LayoutInfo;

import javafx.geometry.VPos;

class ImageButton extends ImageView {
	public function action(): Void {}
	override var onMouseClicked = function (e: MouseEvent) { action(); }
	override var preserveRatio = true;
	override var cache = true;
}

class ToolBarButton extends Button {
	public var control: CarneadesControl;
	override var layoutInfo = LayoutInfo {
		minHeight: toolBarHeight - 10
		height: toolBarHeight - 10
		
	}
}



/**
* The class for the upper toolbar of the standard view.
*/
public class ToolBar extends Panel {
	public var control: CarneadesControl;

	var debugButton: Button = Button {
		text: "debug"
		onMouseClicked: function(e: MouseEvent): Void {
		}
	}

	var openButton: ToolBarButton = ToolBarButton {
		text: "open"
		control: bind control
		action: function(): Void { control.open(); }
	};

	var saveButton: ToolBarButton = ToolBarButton {
		text: "save"
		disable: bind not control.fileChanged
		control: bind control
		action: function(): Void { control.save(); }
	};

	var saveAsButton: ToolBarButton = ToolBarButton {
		text: "save as"
		control: bind control
		action: function(): Void { control.saveAs(); }
	};

	var undoButton: ToolBarButton = ToolBarButton {
		text: "undo"
		disable: bind not control.possibleToUndo
		control: bind control
		action: function(): Void { control.undo(); }
	};

	var redoButton: ToolBarButton = ToolBarButton {
		text: "redo"
		disable: bind not control.possibleToRedo
		control: bind control
		action: function(): Void { control.redo(); }
	};

	var quitButton: ToolBarButton = ToolBarButton {
		text: "quit"
		control: bind control
		action: function(): Void { control.quit(); }
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
				quitButton
			]
		}
	];
}

